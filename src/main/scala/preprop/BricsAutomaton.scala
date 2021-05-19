/*
 * This file is part of Sloth, an SMT solver for strings.
 * Copyright (C) 2018  Matthew Hague, Philipp Ruemmer
 *
 * This program is free software: you can redistribute it and/or modify
 * it under the terms of the GNU General Public License as published by
 * the Free Software Foundation, either version 3 of the License, or
 * (at your option) any later version.
 *
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU General Public License for more details.
 *
 * You should have received a copy of the GNU General Public License
 * along with this program.  If not, see <https://www.gnu.org/licenses/>.
 */

package strsolver.preprop

import ap.parser.{IConstant, ITerm, InputAbsy2Internal}
import ap.terfor.Term
import ap.terfor.preds.PredConj
import dk.brics.automaton.{BasicAutomata, BasicOperations, RegExp, Transition, Automaton => BAutomaton, State => BState}
import strsolver.Regex2AFA

import scala.collection.JavaConversions.{asScalaIterator, iterableAsScalaIterable}
import scala.collection.immutable.List
import scala.collection.mutable.{ArrayBuffer, HashMap => MHashMap, HashSet => MHashSet, LinkedHashSet => MLinkedHashSet, MultiMap => MMultiMap, Set => MSet, Stack => MStack, TreeSet => MTreeSet}

object BricsAutomaton {
  private def toBAutomaton(aut : Automaton) : BAutomaton = aut match {
    case that : BricsAutomaton =>
      that.underlying
    case that : AtomicStateAutomatonAdapter[_] =>
      toBAutomaton(that.internalise)
    case _ =>
      throw new IllegalArgumentException
  }

  def apply(c : Term, context : PredConj) : BricsAutomaton = {
    val converter = new Regex2AFA(context)
    new BricsAutomaton(converter.buildBricsAut(c))
  }

  def apply() : BricsAutomaton = new BricsAutomaton(new BAutomaton)

  /**
   * Build brics automaton from a regular expression in brics format
   */
  def apply(pattern: String): BricsAutomaton =
    new BricsAutomaton(new RegExp(pattern).toAutomaton(true))

  /**
   * Build brics automaton that accepts exactly the given word
   */
  def fromString(str : String) : BricsAutomaton =
    new BricsAutomaton(BasicAutomata makeString str)

  /**
   * A new automaton that accepts any string
   */
  def makeAnyString() : BricsAutomaton = {
    val builder = new BricsAutomatonBuilder
    val initState = builder.getNewState
    val Sigma = builder.LabelOps.sigmaLabel
    builder.setInitialState(initState)
    builder.setAccept(initState, true)
    builder.addTransition(initState, Sigma, initState)
    builder.getAutomaton

  }


  def isLenAut(aut : BricsAutomaton) : Boolean = {
    aut.etaMap.values.forall{
      case l => l==List(1)
    }
  }
  def isNormalAut(aut : BricsAutomaton) : Boolean = {
    aut.registers.size==0
  }
  def isOtherAut(aut : BricsAutomaton) : Boolean = {
    !isLenAut(aut) && !isNormalAut(aut)
  }

  def lenAutsProduct(auts : Seq[Automaton], storeLC : StoreLC) : BricsAutomaton = {
    val autStand = auts(0).asInstanceOf[BricsAutomaton]
    val tmpaut = auts.reduceLeft(_&_).asInstanceOf[BricsAutomaton]
    if(auts.size <= 0 )
      throw new Exception("auts size is 0")
    val b = autStand.getBuilder
    val states = List.fill(tmpaut.states.size)(b.getNewState)
    val statesmap = tmpaut.states.zip(states).toMap
    b.setInitialState(statesmap(tmpaut.initialState))
    val autsRegSet = new MHashSet[ITerm]()
    auts.foreach{
      case a => autsRegSet ++= a.registers.toSet
    }
    for(s <- tmpaut.states){
      if(tmpaut.isAccept(s)){
        b.setAccept(statesmap(s), true)
      }
      for((to, label) <- tmpaut.outgoingTransitions(s)){
        if(autsRegSet.size>0)
          // have registers
          b.addTransition(statesmap(s), label, statesmap(to), List(1))
        else
          // do not have registers
          b.addTransition(statesmap(s), label, statesmap(to))
      }
    }
    val res = b.getAutomaton
    res.addEtaMaps(b.etaMap)
    if(autsRegSet.size > 0) {
      // have registers
      res.addRegisters(autsRegSet.toList.takeRight(1))
      autsRegSet.foreach {
        case reg => storeLC.addFormula(res.registers(0) === reg)
      }
    }
    res
  }
  /**
    * concatenate
    */
  def concat(aut1: BricsAutomaton, aut2: BricsAutomaton) : BricsAutomaton = {
    if(aut1.registers.size==0 && aut2.registers.size==0) {
      // normal concat, all auts do not contain register
      val Baut1 = aut1.underlying
      val Baut2 = aut2.underlying
      val aut = BasicOperations.concatenate(Baut1, Baut2)
      aut.minimize()
      aut.restoreInvariant
      new BricsAutomaton(aut)
    }else{
      val aut2containEpsilon = aut2.underlying.getShortestExample(true) == ""
      // some auts contain registers
      val r1len = aut1.registers.size
      val tmplist1 = List.fill(r1len)(0)
      val r2len = aut2.registers.size
      val tmplist2 = List.fill(r2len)(0)
      val b = aut1.getBuilder
      val newstates = List.fill(aut1.states.size+aut2.states.size)(b.getNewState)
      val statesmap = (aut1.states.toList++aut2.states.toList).zip(newstates).toMap
      b.setInitialState(statesmap(aut1.initialState))
      for(s1 <- aut1.states){
        for((to, label) <- aut1.outgoingTransitions(s1)){
          val vector = aut1.etaMap((s1,label,to))
          b.addTransition(statesmap(s1), label, statesmap(to), vector++tmplist2)
        }
        if(aut1.isAccept(s1)){
          if(aut2containEpsilon)
            b.setAccept(statesmap(s1), true)
          for((to,label) <- aut2.outgoingTransitions(aut2.initialState)){
            val vector = aut2.etaMap((aut2.initialState, label, to))
            b.addTransition(statesmap(s1), label, statesmap(to), tmplist1++vector)
          }
        }
      }
      var aut2InitArrivable = false
      val aut2Init = aut2.initialState
      for((_, _, to) <- aut2.transitions){
        if(to == aut2Init)
          aut2InitArrivable = true
      }
      for(s2 <- aut2.states){
        if(aut2.isAccept(s2)){
          b.setAccept(statesmap(s2), true)
        }
        if(aut2InitArrivable || s2!=aut2Init) {
          for ((to, label) <- aut2.outgoingTransitions(s2)) {
            val vector = aut2.etaMap((s2, label, to))
            b.addTransition(statesmap(s2), label, statesmap(to), tmplist1 ++ vector)
          }
        }
      }
      val res = b.getAutomaton
      res.addEtaMaps(b.etaMap)
      res.addRegisters(aut1.registers++aut2.registers)
      res
    }
  }
  def concat(auts: List[BricsAutomaton]) : BricsAutomaton = {
    auts.reduceLeft(concat(_,_))
  }

  // get product of auts whose registers is NULL
  def productNormally(auts: Seq[Automaton]) : BricsAutomaton = {
    new BricsAutomaton(toBAutomaton(auts.reduceLeft(_ & _)))
  }

  val MaxSimultaneousProduct = 5

  def product(auts : Seq[BricsAutomaton]) : BricsAutomaton = {
    if (auts.size == 0)
      return (BricsAutomaton.makeAnyString)

    if (auts.size == 1)
      return auts.head

    val firstSlice = auts.grouped(MaxSimultaneousProduct)
                         .map(fullProduct(_))
                         .toSeq

    if (firstSlice.size == 1)
      return firstSlice(0)
    else
      return product(firstSlice)
  }

  def fullProduct(auts : Seq[BricsAutomaton]) : BricsAutomaton = {
    val head = auts.head
    val builder = head.getBuilder

    val registers  = auts.flatMap(_.registers).toList

    val initState = builder.initialState
    // from new states to list of old 
    val sMap = new MHashMap[head.State, List[Any]]
    // from list of old to new states
    val sMapRev = new MHashMap[List[Any], head.State]

    val initStates = (auts.map(_.initialState)).toList
    sMap += initState -> initStates
    sMapRev += initStates -> initState

    val worklist = new MStack[(head.State, List[Any])]
    worklist push ((initState, initStates))

    val seenlist = MHashSet[List[Any]]()
    seenlist += initStates

    builder.setAccept(initState,
                      auts forall { aut => aut.isAccept(aut.initialState) })

        var checkCnt = 0

    while (!worklist.isEmpty) {
      val (ps, ss) = worklist.pop()

      // collects transitions from (s, ss)
      // lbl = (min, max) is current range
      // vector is current vector
      // sp and ssp are s' and ss' (ss' is reversed for efficiency)
      // ss are elements of ss from which a transition is yet to be
      // searched
      def addTransitions(lbl : head.TLabel,
                         vector : List[Int],
                         ssp : List[Any],
                         remAuts : List[BricsAutomaton],
                         ss : List[Any]) : Unit = {
        checkCnt = checkCnt + 1
        if (checkCnt % 1000 == 0)
          ap.util.Timeout.check
        ss match {
          case Seq() =>  {
            val nextState = ssp.reverse
            if (!seenlist.contains(nextState)) {
                val nextPState = builder.getNewState
                val isAccept = (auts.iterator zip nextState.iterator) forall {
                  case (aut, s) => aut.isAccept(s.asInstanceOf[aut.State])
                }
                builder.setAccept(nextPState, isAccept)
                sMap += nextPState -> nextState
                sMapRev += nextState -> nextPState
                worklist.push((nextPState, nextState))
                seenlist += nextState
            }
            val nextPState = sMapRev(nextState)
            // add vector
            builder.addTransition(ps, lbl, nextPState, vector)
          }
          case _state :: ssTail => {
            val aut :: autsTail = remAuts
            val state = _state.asInstanceOf[aut.State]

            aut.outgoingTransitions(state) foreach {
              case (s, nextLbl) => {
                val newLbl =
                    builder.LabelOps.intersectLabels(lbl,
                                                     nextLbl.asInstanceOf[head.TLabel])
                  // this sort maybe wrong
                val vect = vector ::: aut.etaMap((state, nextLbl, s))
                for (l <- newLbl)
                  addTransitions(l, vect, s::ssp, autsTail, ssTail)
              }
            }
          }
        }
      }

      addTransitions(builder.LabelOps.sigmaLabel, List(), List(), auts.toList, ss)
    }

    val res = builder.getAutomaton
    res.addEtaMaps(builder.etaMap)
    res.setRegisters(registers)
    res
  }
}

object BricsTLabelOps extends TLabelOps[(Char, Char)] {

  val vocabularyWidth : Int = 16  // really?

  /**
   * Check whether the given label accepts any letter
   */
  def isNonEmptyLabel(label : (Char, Char)) : Boolean =
    label._1 <= label._2

  /**
   * Label accepting all letters
   */
  val sigmaLabel : (Char, Char) =
    (Util.CharMin, Util.CharMax)


  def singleton(a : Char) = (a, a)

  /**
   * Intersection of two labels
   */
  def intersectLabels(l1 : (Char, Char),
                      l2 : (Char, Char)) : Option[(Char, Char)] = {
    Option(l1._1 max l2._1, l1._2 min l2._2).filter(isNonEmptyLabel(_))
  }

  /**
   * True if labels overlap
   */
  def labelsOverlap(l1 : (Char, Char),
                    l2 : (Char, Char)) : Boolean = {
    val (min1, max1) = l1
    val (min2, max2) = l2
    (min2 <= max1 && max2 >= min1)
  }

  /**
   * Can l represent a?
   */
  def labelContains(a : Char, l : (Char, Char)) : Boolean = {
    val (min, max) = l
    (min <= a && a <= max)
  }

  /**
   * Enumerate all letters accepted by a transition label
   */
  def enumLetters(label : (Char, Char)) : Iterator[Int] =
    for (c <- (label._1 to label._2).iterator) yield c.toInt

  /**
   * Remove a given character from the label.  E.g. [1,10] - 5 is
   * [1,4],[6,10]
   */
  def subtractLetter(a : Char, lbl : (Char, Char)) : Iterable[(Char, Char)] = {
    val (min, max) = lbl
    if (min <= a && a <= max) {
      // surely a cuter solution than this exists...
      var res = List[(Char, Char)]()
      if (min < a)
        res = (min, (a-1).toChar)::res
      if (a < max)
        res = ((a+1).toChar, max)::res
      res
    } else {
      Seq(lbl)
    }
  }


  /**
   * Shift characters by n, do not wrap.  E.g. [1,2].shift 3 = [4,5]
   */
  def shift(lbl : (Char, Char), n : Int) : (Char, Char) = {
    val (cmin, cmax) = lbl
    var left = Math.max(Util.CharMin, cmin + n)   // e.g [1,3] shift -2 = [0, 1]
    var right = Math.min(Util.CharMax, cmax + n)
    if(right < Util.CharMin || left > Util.CharMax){
      // e.g [1,3] shift -4 = None. [126,127] shift +3 = None
      // illegal label
      left = 10
      right = 0
    }
    (left.toChar, right.toChar)
  }

  /**
   * Get representation of interval [min,max]
   */
  def interval(min : Char, max : Char) : (Char, Char) = (min, max)
}

class BricsTLabelEnumerator(labels: Iterator[(Char, Char)])
    extends TLabelEnumerator[(Char, Char)] {
  /**
   * Keep track of disjoint labels for fast range lookups in
   * enumLabelOverlap.  Access with getDisjointLabels.
   */
  private lazy val disjointLabels : MTreeSet[(Char, Char)] =
    calculateDisjointLabels
  /**
   * Like disjoint labels but covers whole alphabet including internal
   * char.
   */
  private lazy val disjointLabelsComplete : List[(Char, Char)] =
    calculateDisjointLabelsComplete

  /**
   * Enumerate all labels with overlaps removed.
   * E.g. for min/max labels [1,3] [5,10] [8,15] would result in [1,3]
   * [5,8] [8,10] [10,15]
   */
  def enumDisjointLabels : Iterable[(Char, Char)] =
    disjointLabels.toIterable

  /**
   * Enumerate all labels with overlaps removed such that the whole
   * alphabet is covered (including internal characters)
   * E.g. for min/max labels [1,3] [5,10] [8,15] would result in [1,3]
   * [4,4] [5,7] [8,10] [11,15] [15,..]
   */
  def enumDisjointLabelsComplete : Iterable[(Char, Char)] =
    disjointLabelsComplete

  /**
   * iterate over the instances of lbls that overlap with lbl
   */
  def enumLabelOverlap(lbl : (Char, Char)) : Iterable[(Char, Char)] = {
    val (lMin, lMax) = lbl
    disjointLabels.
      from((lMin, Util.CharMin)).
      to((lMax, Util.CharMax)).
      toIterable
  }

  /**
   * Takes disjoint enumeration and splits it at the point defined by
   * Char.  E.g. [1,10] split at 5 is [1,4][5][6,10]
   */
  def split(a : Char) : TLabelEnumerator[(Char, Char)] =
    new BricsTLabelEnumerator(disjointLabels.iterator ++ Iterator((a, a)))

  private def calculateDisjointLabels() : MTreeSet[(Char,Char)] = {
    var disjoint = new MTreeSet[(Char, Char)]()

    val mins = new MTreeSet[Char]
    val maxes = new MTreeSet[Char]
    for ((min, max) <- labels) {
      mins += min
      maxes += max
    }

    val imin = mins.iterator
    val imax = maxes.iterator

    if (!imin.hasNext)
      return disjoint

    var curMin = imin.next
    var nextMax = imax.next
    while (imin.hasNext) {
      val nextMin = imin.next
      if (nextMin <= nextMax) {
        disjoint += ((curMin, (nextMin-1).toChar))
        curMin = nextMin
      } else {
        disjoint += ((curMin, nextMax))
        curMin = nextMin
        nextMax = imax.next
      }
    }

    disjoint += ((curMin, nextMax))
    curMin = (nextMax + 1).toChar

    while (imax.hasNext) {
      val nextMax = imax.next
      disjoint += ((curMin, nextMax))
      curMin = (nextMax + 1).toChar
    }

    disjoint
  }

  private def calculateDisjointLabelsComplete() : List[(Char, Char)] = {
    val labelsComp = disjointLabels.foldRight(List[(Char, Char)]()) {
      case ((min, max), Nil) => {
        // using Util.CharMax since we include internal chars
        if (max < Util.CharMax)
          List((min,max), ((max + 1).toChar, Util.CharMax))
        else
          List((min, max))
      }
      case ((min, max), (minLast, maxLast)::lbls) => {
        val minLastPrev = (minLast - 1).toChar
        if (max < minLastPrev)
          (min, max)::((max + 1).toChar, minLastPrev)::(minLast, maxLast)::lbls
        else
          (min, max)::(minLast, maxLast)::lbls
      }
    }
    if (Util.CharMin < labelsComp.head._1) {
      val nextMin = (labelsComp.head._1 - 1).toChar
      (Util.CharMin, nextMin)::labelsComp
    } else {
      labelsComp
    }
  }
}

/**
 * Wrapper for the BRICS automaton class
 */
class BricsAutomaton(val underlying : BAutomaton) extends AtomicStateAutomaton {

  import BricsAutomaton.toBAutomaton

  type State = BState
  type TLabel = (Char, Char)

  override val LabelOps = BricsTLabelOps

  override def toString : String = underlying.toString

  // (q', σ, q, η) tuple, stored as a map
  val etaMap = new MHashMap[(State, TLabel, State), List[Int]]

  addEtaMapsDefault

  /**
   * get parikh iamge of BricsAutomaton
   */
  import ap.PresburgerTools
  import ap.basetypes.IdealInt
  import ap.terfor.conjunctions.{Conjunction, ReduceWithConjunction}
  import ap.terfor.linearcombination.LinearCombination
  import ap.terfor.{Formula, OneTerm, TerForConvenience, TermOrder}

  import scala.collection.mutable.{BitSet => MBitSet, HashMap => MHashMap}

  override def parikhImage : Formula = Exploration.measure("length abstraction") {
    import TerForConvenience._
    
    val constantSeq = registers.map{case IConstant(c) => c}
    implicit val order = TermOrder.EMPTY.extend(constantSeq)

    // val a = registers.map(InputAbsy2Internal(_,TermOrder.EMPTY)).toSeq

    val stateSeq = states.toIndexedSeq
    val state2Index = stateSeq.iterator.zipWithIndex.toMap

    lazy val preStates = {
      val preStates = Array.fill(stateSeq.size)(new ArrayBuffer[(Int, List[Int])])

      for ((from, label, to) <- transitions){
        val vector = etaMap((from, label, to))
        preStates(state2Index(to)) += ((state2Index(from), vector))
      }

      preStates
    }
    lazy val transPreStates = {
      val transPreStates = Array.fill(stateSeq.size)(new MBitSet)

      // transPreStates clone from preStates
      for ((array, s2) <- preStates.iterator zip transPreStates.iterator)
        for((s1 , _) <- array)
          s2 += s1

      for ((s, n) <- transPreStates.iterator.zipWithIndex)
        s += n

      var changed = true
      while (changed) {
        changed = false

        for (i <- 0 until transPreStates.size) {
          val set = transPreStates(i)

          val oldSize = set.size
          for (j <- 0 until transPreStates.size)
            if (set contains j)
              set |= transPreStates(j)

          if (set.size > oldSize)
            changed = true
        }
      }

      transPreStates
    }  
    val initialStateInd = state2Index(initialState)

    ////////////////////////////////////////////////////////////////////////////

    // disjunction of formulas
    disjFor(for (finalState <- acceptingStates)
            yield  {
      val finalStateInd = state2Index(finalState)
      val refStates = transPreStates(finalStateInd)

      val productions : List[(Int, Option[Int], List[Int])] =
        (if (refStates contains initialStateInd)
           List((initialStateInd, None, List()))
         else List()) :::
        (for (state <- refStates.iterator;
              preState <- preStates(state))
         yield (state, Some(preState._1), preState._2)).toList

      val (prodVars, zVars, sizeVar) = {
        val prodVars = for ((_, num) <- productions.zipWithIndex) yield v(num)
        var nextVar = prodVars.size
        val zVars = (for (state <- refStates.iterator) yield {
          val ind = nextVar
          nextVar = nextVar + 1
          state -> v(ind)
        }).toMap
        (prodVars, zVars, v(nextVar))
      }

      // equations relating the production counters
      // consistent
      val prodEqs =
        (for (state <- refStates.iterator) yield {
          LinearCombination(
             (if (state == finalStateInd)
                Iterator((IdealInt.ONE, OneTerm))
              else
                Iterator.empty) ++
            
                (for (((to, from, _), prodVar) <-
                         productions.iterator zip prodVars.iterator;
                       mult = (if (from contains state) 1 else 0) -
                              (if (to == state) 1 else 0))
                 yield (IdealInt(mult), prodVar)),
             order)
        }).toList

      // registers
      val rEqs =
        (for((r,i) <- registers.zipWithIndex) yield {
          LinearCombination(
            Iterator((IdealInt.MINUS_ONE, InputAbsy2Internal(r,order)))
            ++
            (for(((_, from, vector), prodVar) <- productions.iterator zip prodVars.iterator 
             
              if from != None )
              yield {(IdealInt(vector(i)), prodVar)}),
          order)
        }).toList

      val entryZEq =
        zVars(finalStateInd) - 1

      val allEqs = eqZ(entryZEq :: prodEqs ::: rEqs)

      val prodNonNeg =
        prodVars >= 0

      val prodImps =
        (for (((to, _, _), prodVar) <-
                productions.iterator zip prodVars.iterator;
              if to != finalStateInd)
         yield ((prodVar === 0) | (zVars(to) > 0))).toList

      // connective
      val zImps =
        (for (state <- refStates.iterator; if state != finalStateInd) yield {
           disjFor(Iterator(zVars(state) === 0) ++
                   (for (((to, from, _), prodVar) <-
                           productions.iterator zip prodVars.iterator;
                         if from contains state)
                    yield conj(zVars(state) === zVars(to) + 1,
                               geqZ(List(prodVar - 1, zVars(to) - 1)))))
         }).toList

      val matrix =
        conj(allEqs :: prodNonNeg :: prodImps ::: zImps)
      val rawConstraint =
        exists(prodVars.size + zVars.size, matrix)

      val constraint =
        ap.util.Timeout.withTimeoutMillis(1000) {
          // best-effort attempt to get a quantifier-free version of the
          // length abstraction
          PresburgerTools.elimQuantifiersWithPreds(rawConstraint)
        } {
          ReduceWithConjunction(Conjunction.TRUE, order)(rawConstraint)
        }

      constraint
    })
  }
  
  /**
   * addEtaMap, vector.length must equal to registers.length
   */
  def addEtaMap(transition : (State, TLabel, State), vector : List[Int]) : Unit = {
    etaMap += (transition -> vector)
  }


  /**
   * addEtaMaps, input is a hashmap
   */
  def addEtaMaps(map : MHashMap[(State, TLabel, State), List[Int]]) : Unit = {
    etaMap ++= map
  }

  /**
   * addEtaMapDefault, the default vector is considered to be 0 
   */
  def addEtaMapDefault(transition : (State, TLabel, State)) : Unit = {
    val vector = List.fill(registers.length)(0)
    addEtaMap(transition, vector)
  }

  /**
   * addEtaMapsDefault
   */
  def addEtaMapsDefault() : Unit = {
    for((s1, l, s2) <- this.transitions){
      addEtaMapDefault((s1, l, s2))
    }
  }
  /**
   * register op:
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
  def deleteTransition(q1 : BricsAutomaton#State,
                       label : BricsAutomaton#TLabel,
                       q2 : BricsAutomaton#State) : Unit = {
    etaMap -= ((q1, label, q2))
  }
  def removeDeadTranstions() : Unit = {
    for(((from, label, to),v) <- etaMap){
      if(!states.toSet.contains(from))
        deleteTransition(from, label, to)
    }
  }

  /**
   * Union
   */
  def |(that : Automaton) : Automaton =
    new BricsAutomaton(BasicOperations.union(this.underlying,
                                             toBAutomaton(that)))

  /**
   * Intersection
   */
  def &(that : Automaton) : Automaton = {
    val productAut = BasicOperations.intersection(this.underlying, toBAutomaton(that))
    productAut.minimize()
    new BricsAutomaton(productAut)
  }

  /**
   * Complementation
   */
  def unary_! : Automaton =
    new BricsAutomaton(BasicOperations.complement(this.underlying))


  /**
   * Check whether this automaton describes the empty language.
   */
  def isEmpty : Boolean =
    underlying.isEmpty

  /**
   * Check whether the automaton accepts a given word.
   */
  def apply(word : Seq[Int]) : Boolean =
    BasicOperations.run(
      this.underlying,
      SeqCharSequence(for (c <- word.toIndexedSeq) yield c.toChar).toString)

  /**
   * Iterate over automaton states, return in deterministic order
   */
  lazy val states : Iterable[State] = {
    // do this the hard way to give a deterministic ordering
    val worklist = new MStack[State]
    val seenstates = new MLinkedHashSet[State]

    worklist.push(initialState)
    seenstates.add(initialState)

    while(!worklist.isEmpty) {
      val s = worklist.pop


      for ((to, _) <- outgoingTransitions(s)) {
        if (!seenstates.contains(to)) {
          worklist.push(to)
          seenstates += to
        }
      }
    }

    seenstates
  }

  /**
   * The unique initial state
   */
  lazy val initialState : State = underlying.getInitialState

  /**
   * Given a state, iterate over all outgoing transitions, try to be
   * deterministic
   */
  def outgoingTransitions(from : State) : Iterator[(State, TLabel)] = {
    val dests = new MHashMap[TLabel, MSet[State]] with MMultiMap[TLabel, State]

    for (t <- from.getTransitions)
      dests.addBinding((t.getMin, t.getMax), t.getDest)

    val outgoing = new MLinkedHashSet[(State, TLabel)]

    for (lbl <- dests.keys.toList.sorted) {
      for (s <- dests(lbl)) {
        outgoing += ((s, lbl))
      }
    }

    outgoing.iterator
  }

  /**
   * The set of accepting states
   */
  lazy val acceptingStates : Set[State] =
    (for (s <- states; if s.isAccept) yield s).toSet

  lazy val labelEnumerator =
    new BricsTLabelEnumerator(for ((_, lbl, _) <- transitions) yield lbl)

  /*
   * Get any word accepted by this automaton, or <code>None</code>
   * if the language is empty
   */
  def getAcceptedWord : Option[Seq[Int]] =
    (this.underlying getShortestExample true) match {
      case null => None
      case str  => Some(for (c <- str) yield c.toInt)
    }

  def isAccept(s : State) : Boolean = s.isAccept

  def toDetailedString : String = underlying.toString

  def getBuilder : BricsAutomatonBuilder = new BricsAutomatonBuilder

  def getTransducerBuilder : BricsTransducerBuilder = BricsTransducer.getBuilder
}


/**
 * For constructing manually (immutable) BricsAutomaton objects
 */
class BricsAutomatonBuilder
    extends AtomicStateAutomatonBuilder[BricsAutomaton#State,
                                        BricsAutomaton#TLabel] {
  val LabelOps : TLabelOps[BricsAutomaton#TLabel] = BricsTLabelOps

  var minimize = true

  val baut : BAutomaton = {
    val baut = new BAutomaton
    baut.setDeterministic(false)
    baut
  }

  val etaMap = new MHashMap[(BricsAutomaton#State, BricsAutomaton#TLabel, 
                              BricsAutomaton#State), List[Int]]

  /**
   * The initial state of the automaton being built
   */
  def initialState : BricsAutomaton#State = baut.getInitialState

  /**
   * By default one can assume built automata are minimised before the
   * are returned.  Use this to enable or disable it
   */
  def setMinimize(minimize : Boolean) : Unit = { this.minimize = minimize }

  /**
   * Create a fresh state that can be used in the automaton
   */
  def getNewState : BricsAutomaton#State = new BState()

  /**
   * Set the initial state
   */
  def setInitialState(q : BricsAutomaton#State) : Unit =
    baut.setInitialState(q)

  /**
   * Add a new transition q1 --label--> q2
   */
  def addTransition(q1 : BricsAutomaton#State,
                    label : BricsAutomaton#TLabel,
                    q2 : BricsAutomaton#State) : Unit = {
    if (LabelOps.isNonEmptyLabel(label)) {
      val (min, max) = label
      q1.addTransition(new Transition(min, max, q2))
    }
  }

  // huzi add-----------------------------------------
  def addTransition(q1 : BricsAutomaton#State,
                    label : BricsAutomaton#TLabel,
                    q2 : BricsAutomaton#State,
                    vector : List[Int] ) : Unit = {
    if (LabelOps.isNonEmptyLabel(label)) {
      val (min, max) = label
      q1.addTransition(new Transition(min, max, q2))
      etaMap += ((q1, (min, max), q2)->vector)
    }
  }



  def outgoingTransitions(q : BricsAutomaton#State)
        : Iterator[(BricsAutomaton#State, BricsAutomaton#TLabel)] =
    for (t <- q.getTransitions.iterator)
      yield (t.getDest, (t.getMin, t.getMax))

  def setAccept(q : BricsAutomaton#State, isAccepting : Boolean) : Unit =
    q.setAccept(isAccepting)

  def isAccept(q : BricsAutomaton#State) : Boolean = q.isAccept

  /**
   * Returns built automaton.  Can only be used once after which the
   * automaton cannot change
   */
  def getAutomaton : BricsAutomaton = {
    new BricsAutomaton(baut)
  }
}


