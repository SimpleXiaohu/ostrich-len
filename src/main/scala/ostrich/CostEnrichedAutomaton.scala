/**
 * This file is part of Ostrich, an SMT solver for strings.
 * Copyright (c) 2021 Matthew Hague, Philipp Ruemmer. All rights reserved.
 * 
 * Redistribution and use in source and binary forms, with or without
 * modification, are permitted provided that the following conditions are met:
 * 
 * * Redistributions of source code must retain the above copyright notice, this
 *   list of conditions and the following disclaimer.
 * 
 * * Redistributions in binary form must reproduce the above copyright notice,
 *   this list of conditions and the following disclaimer in the documentation
 *   and/or other materials provided with the distribution.
 * 
 * * Neither the name of the authors nor the names of their
 *   contributors may be used to endorse or promote products derived from
 *   this software without specific prior written permission.
 * 
 * THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS
 * "AS IS" AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT
 * LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS
 * FOR A PARTICULAR PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL THE
 * COPYRIGHT HOLDER OR CONTRIBUTORS BE LIABLE FOR ANY DIRECT,
 * INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES
 * (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR
 * SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION)
 * HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT,
 * STRICT LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE)
 * ARISING IN ANY WAY OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED
 * OF THE POSSIBILITY OF SUCH DAMAGE.
 */

package ostrich

import ap.parser.ITerm
import ap.terfor.Formula
import dk.brics.automaton.{State => BState}

import scala.collection.immutable.List
import scala.collection.mutable.{ArrayBuffer, HashMap, HashSet, Stack}

/**
 * Trait for automata with atomic/nominal states; i.e., states
 * don't have any structure and are not composite, there is a unique
 * initial state, and a set of accepting states.
 */
trait CostEnrichedAutomaton extends AtomicStateAutomaton {

  type State = BState
  type TLabel = (Char, Char)

  /**
   * Union. This method assumed that <code>this</code> and <code>that</code>
   * have the same number of cost counters.
   */
  def |(that : CostEnrichedAutomaton) : CostEnrichedAutomaton

  /**
   * Intersection. The resulting automata will have
   * <code>this.counterNum + that.counterNum</code> counters.
   */
  def &(that : CostEnrichedAutomaton) : CostEnrichedAutomaton

  /**
   * registers (i.e. counters of CostEnrichedAutomaton)
   */
  val registers : ArrayBuffer[ITerm]= ArrayBuffer()

  /**
   * registers op:
   */
  def addNewRegister(num : Int): Unit = {
    for(i <- 1 to num)
      registers += AllocRegisterTerm()
  }
  def addRegisters(rs : Seq[ITerm]): Unit = {
    registers ++= rs
  }
  def setRegisters (rs : ArrayBuffer[ITerm]): Unit = {
    registers.clear()
    registers ++= rs
  }
  def setRegisters (rs : List[ITerm]): Unit = {
    registers.clear()
    registers ++= rs
  }
  /**
   * cloneRegisters : clone register, use the different token
   */
  def cloneRegisters(): ArrayBuffer[ITerm]  = {
    val res : ArrayBuffer[ITerm]= ArrayBuffer()
    for(i<-registers){
      res += AllocRegisterTerm()
    }
    res
  }

  /**
   * Given a state, iterate over all outgoing transitions, including
   * their label and the costs.
   */
  def outgoingTransitionsWithCost(from : State)
                                : Iterator[(State, TLabel, List[Int])]
  
  def outgoingTransitions(from : State) : Iterator[(State, TLabel)] =
    for ((s, l, _) <- outgoingTransitionsWithCost(from)) yield (s, l)

  /**
   * Return new automaton builder of compatible type
   */
  def getBuilder : CostEnrichedAutomatonBuilder[State, TLabel]

  /**
   * Compute the set of possible cost vectors as a Presburger formula.
   * The costs are represented by the variables <code>_0, _1, ...</code>.
   */
  def getCostImage : Formula

  //////////////////////////////////////////////////////////////////////////
  // Derived methods

  /**
   * Iterate over all transitions with costs
   */
  def transitionsWithCost : Iterator[(State, TLabel, List[Int], State)] =
    for (s1 <- states.iterator; (s2, lbl, c) <- outgoingTransitionsWithCost(s1))
      yield (s1, lbl, c, s2)

}

trait CostEnrichedAutomatonBuilder[State, TLabel]
      extends AtomicStateAutomatonBuilder[State, TLabel] {

  /**
   * Set the number of cost counters used in the automaton. This
   * method can only be used prior to adding states or transitions.
   */
  def setCounterNum(num : Int) : Unit

  /**
   * Add a new transition q1 --label--> q2 with given cost
   */
  def addTransitionWithCost(s1 : State,
                            label : TLabel, costs : Seq[Int],
                            s2 : State) : Unit

  /**
   * Iterate over outgoing transitions from state, with their costs
   */
  def outgoingTransitionsWithCost(q : State) : Iterator[(State, TLabel, Seq[Int])]

  /**
   * Returns built automaton.  Can only be used once after which the
   * automaton cannot change
   */
  def getAutomaton : CostEnrichedAutomaton

}


class  CEAutomaton extends CostEnrichedAutomaton {

  /**
   * Union
   */
  def |(that : CostEnrichedAutomaton) : CostEnrichedAutomaton = {
    // wrong implementation, have not defined
    that
  }

  /**
   * Intersection
   */
  def &(that : CostEnrichedAutomaton) : CostEnrichedAutomaton = {
    val builder = that.getBuilder
    // from new state to old states pair
    val sMap = new HashMap[that.State, Seq[Any]]
    // from old states pair to new state
    val sMapRev = new HashMap[Seq[Any], that.State]
    val initState = builder.initialState
    sMap += initState -> Seq(this.initialState, that.initialState)
    sMapRev += Seq(this.initialState, that.initialState) -> initState

    if(this.isAccept(this.initialState) && that.isAccept(that.initialState)){
      builder.setAccept(initState, true)
    }

    val workList = new Stack[(that.State, Seq[Any])]()
    workList push ((initState, Seq(this.initialState, that.initialState)))
    val seenList = new HashSet[Seq[Any]]()
    seenList += Seq(this.initialState, that.initialState)

    while (!workList.isEmpty){
      val (state, statePair) = workList.pop()
      val oldState1 = statePair(0).asInstanceOf[that.State]
      val oldState2 = statePair(1).asInstanceOf[that.State]
      for ((state1Prim, lb1, vector1) <- outgoingTransitionsWithCost(oldState1)){
        for ((state2Prim, lb2, vector2) <- outgoingTransitionsWithCost(oldState2)){
          val newLbList = builder.LabelOps.intersectLabels(lb1,lb2)
          for (lb <- newLbList){
            if(!seenList.contains(Seq(state1Prim, state2Prim))){
              seenList += Seq(state1Prim, state2Prim)
              val newState = builder.getNewState
              sMap += newState -> Seq(state1Prim, state2Prim)
              sMapRev += Seq(state1Prim, state2Prim) -> newState
              if(this.isAccept(state1Prim) && that.isAccept(state2Prim)){
                builder.setAccept(newState, true)
              }
              workList push ((newState, Seq(state1Prim, state2Prim)))
            }
            val newState = sMapRev(Seq(state1Prim, state2Prim))
            builder.addTransitionWithCost(state, lb, vector1 ::: vector2, newState)
          }
        }
      }
    }
    builder.minimize()
    builder.getAutomaton

  }
}


class CEAutomatonBuilder
  extends CostEnrichedAutomatonBuilder
            [CostEnrichedAutomaton#State,
              CostEnrichedAutomaton#TLabel] {

}