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

import ap.parser.ITerm
import ap.terfor.Formula
import ap.terfor.conjunctions.Conjunction

import scala.collection.mutable.{ArrayBuffer, ArrayStack, HashMap => MHashMap, HashSet => MHashSet}

/**
 * Interface for different implementations of finite-state automata.
 */
trait Automaton {
  /**
   * Union
   */
  def |(that : Automaton) : Automaton

  /**
   * Intersection
   */
  def &(that : Automaton) : Automaton

  /**
   * Complementation
   */
  def unary_! : Automaton

  /**
   * Check whether this automaton accepts any word.
   */
  def isEmpty : Boolean

  /**
   * Check whether the automaton accepts a given word.
   */
  def apply(word : Seq[Int]) : Boolean

  /**
   * Get any word accepted by this automaton, or <code>None</code>
   * if the language is empty
   */
  def getAcceptedWord : Option[Seq[Int]]

  /**
   * Compute the length abstraction of this automaton.
   */
  // def getLengthAbstraction : Formula

  //huzi add
  /**
   * Compute the parikh image of this automaton
   */
  def parikhImage : Formula = Conjunction.TRUE
  val registers : ArrayBuffer[ITerm]= ArrayBuffer()
}

////////////////////////////////////////////////////////////////////////////////


trait TLabelOps[TLabel] {
  /**
   * Nr. of bits of letters in the vocabulary.
   */
  def vocabularyWidth : Int

  /**
   * Check whether the given label accepts some letter
   */
  def isNonEmptyLabel(label : TLabel) : Boolean

  /**
   * Label accepting all letters
   */
  val sigmaLabel : TLabel

  /**
   * Label representing a single char a
   */
  def singleton(a : Char) : TLabel

  /**
   * Intersection of two labels, None if not overlapping
   */
  def intersectLabels(l1 : TLabel,
                      l2 : TLabel) : Option[TLabel]

  /**
   * True if labels overlap
   */
  def labelsOverlap(l1 : TLabel,
                    l2 : TLabel) : Boolean

  /**
   * Can l represent a?
   */
  def labelContains(a : Char, l : TLabel) : Boolean

  /**
   * Enumerate all letters accepted by a transition label
   */
  def enumLetters(label : TLabel) : Iterator[Int]

  /**
   * Remove a given character from the label.  E.g. [1,10] - 5 is
   * [1,4],[6,10]
   */
  def subtractLetter(a : Char, l : TLabel) : Iterable[TLabel]


  /**
   * Shift characters by n, do not wrap.  E.g. [1,2].shift 3 = [4,5]
   */
  def shift(lbl : TLabel, n : Int) : TLabel

  /**
   * Get representation of interval [min,max]
   */
  def interval(min : Char, max : Char) : TLabel
}

/**
 * A label enumerator is used to enumerate labels appearing in an
 * automaton and derived label sets
 */
trait TLabelEnumerator[TLabel] {
  /**
   * Enumerate all labels with overlaps removed.
   * E.g. for min/max labels [1,3] [5,10] [8,15] would result in [1,3]
   * [5,7] [8,10] [11,15]
   */
  def enumDisjointLabels : Iterable[TLabel]

  /**
   * Enumerate all labels with overlaps removed such that the whole
   * alphabet is covered (including internal characters)
   * E.g. for min/max labels [1,3] [5,10] [8,15] would result in [1,3]
   * [4,4] [5,7] [8,10] [11,15] [15,..]
   */
  def enumDisjointLabelsComplete : Iterable[TLabel]

  /**
   * iterate over disjoint labels of the automaton that overlap with lbl
   */
  def enumLabelOverlap(lbl : TLabel) : Iterable[TLabel]

  /**
   * Takes disjoint enumeration and splits it at the point defined by
   * Char.  E.g. [1,10] split at 5 is [1,4][5][6,10]
   */
  def split(a : Char) : TLabelEnumerator[TLabel]
}

/**
 * Trait for automata with atomic/nominal states; i.e., states
 * don't have any structure and are not composite, there is a unique
 * initial state, and a set of accepting states.
 */
trait AtomicStateAutomaton extends Automaton {
  /**
   * Type of states
   */
  type State

  /**
   * Type of labels
   */
  type TLabel

  /**
   * Operations on labels
   */
  val LabelOps : TLabelOps[TLabel]

  /**
   * Iterate over automaton states
   */
  def states : Iterable[State]

  /**
   * The unique initial state
   */
  val initialState : State

  /**
   * The set of accepting states
   */
  val acceptingStates : Set[State]

  /**
   * To enumerate the labels used
   */
  val labelEnumerator : TLabelEnumerator[TLabel]

  /**
   * Given a state, iterate over all outgoing transitions
   */
  def outgoingTransitions(from : State) : Iterator[(State, TLabel)]

  /**
   * Test if state is accepting
   */
  def isAccept(s : State) : Boolean

  /**
   * Return new automaton builder of compatible type
   */
  def getBuilder : AtomicStateAutomatonBuilder[State, TLabel]

  /**
   * Return new automaton builder of compatible type
   */
  def getTransducerBuilder : TransducerBuilder[State, TLabel]

  /**
   * String representation of automaton in full gory detail
   */
  def toDetailedString : String

  //////////////////////////////////////////////////////////////////////////
  // Derived methods

  /**
   * Iterate over all transitions
   */
  def transitions : Iterator[(State, TLabel, State)] =
    for (s1 <- states.iterator; (s2, lbl) <- outgoingTransitions(s1))
      yield (s1, lbl, s2)

  /**
   * Get image of a set of states under a given label
   */
  def getImage(states : Set[State], lbl : TLabel) : Set[State] = {
    for (s1 <- states;
         (s2, lbl2) <- outgoingTransitions(s1);
         if LabelOps.labelsOverlap(lbl, lbl2))
      yield s2
  }

  /**
   * Get image of a state under a given label
   */
  def getImage(s1 : State, lbl : TLabel) : Set[State] = {
    outgoingTransitions(s1).collect({
      case (s2, lbl2) if (LabelOps.labelsOverlap(lbl, lbl2)) => s2
    }).toSet
  }

  /**
   * Compute states that can only be reached through words with some
   * unique length
   */
  lazy val uniqueLengthStates : Map[State, Int] = {
    val uniqueLengthStates = new MHashMap[State, Int]
    val nonUniqueLengthStates = new MHashSet[State]
    val todo = new ArrayStack[State]

    uniqueLengthStates.put(initialState, 0)
    todo push initialState

    while (!todo.isEmpty) {
      val s = todo.pop
      if (nonUniqueLengthStates contains s) {
        for ((to, _) <- outgoingTransitions(s)) {
          uniqueLengthStates -= to
          if (nonUniqueLengthStates add to)
            todo push to
        }
      } else {
        val sLen = uniqueLengthStates(s)
        for ((to, _) <- outgoingTransitions(s))
          (uniqueLengthStates get to) match {
            case Some(oldLen) =>
              if (oldLen != sLen + 1) {
                uniqueLengthStates -= to
                nonUniqueLengthStates += to
                todo push to
              }
            case None =>
              if (!(nonUniqueLengthStates contains to)) {
                uniqueLengthStates.put(to, sLen + 1)
                todo push to
              }
        }
      }
    }

    uniqueLengthStates.toMap
  }

  /**
   * Field that is defined if the automaton only accepts words of some length l
   * (and the language accepted by the automaton is non-empty)
   */
  lazy val uniqueAcceptedWordLength : Option[Int] = {
    val lengths = for (s <- acceptingStates) yield (uniqueLengthStates get s)
    if (lengths.size == 1 && !(lengths contains None))
      lengths.iterator.next
    else
      None
  }

  
}

trait AtomicStateAutomatonBuilder[State, TLabel] {
  /**
   * Operations on labels
   */
  val LabelOps : TLabelOps[TLabel]

  /**
   * By default one can assume built automata are minimised before the
   * are returned.  Use this to enable or disable it
   */
  def setMinimize(minimize : Boolean) : Unit

  /**
   * The initial state of the automaton being built
   */
  def initialState : State

  /**
   * Create a fresh state that can be used in the automaton
   */
  def getNewState : State

  /**
   * Set the initial state
   */
  def setInitialState(q : State) : Unit

  /**
   * Add a new transition q1 --label--> q2
   */
  def addTransition(s1 : State, label : TLabel, s2 : State) : Unit

  /**
   * Iterate over outgoing transitions from state
   */
  def outgoingTransitions(q : State) : Iterator[(State, TLabel)]

  /**
   * Ask if state is accepting
   */
  def isAccept(q : State) : Boolean

  /**
   * Set state accepting
   */
  def setAccept(s : State, isAccepting : Boolean) : Unit

  /**
   * Returns built automaton.  Can only be used once after which the
   * automaton cannot change
   */
  def getAutomaton : AtomicStateAutomaton
}
