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

import ap.terfor.{Term, Formula, TermOrder, TerForConvenience}
import ap.terfor.preds.PredConj

import dk.brics.automaton.RegExp

import scala.collection.mutable.{HashMap => MHashMap, Stack => MStack}

//TODO : alter all preop, consider the replacement is concreteword
object ReplacePreOpW {
  def apply(a : Char, word : List[Int]) : PreOp = ReplacePreOpWWord(Seq(a), word)

  // /**
  //  * preop for a replace(_, w, word) for whatever we get out of
  //  * PrepropSolver.
  //  */
  def apply(w : List[Either[Int,Term]], word : List[Int]) : PreOp = {
    val charw = w.map(_ match {
      case Left(c) => c.toChar
      case _ =>
        throw new IllegalArgumentException("ReplacePreOpW only supports word or character replacement, got " + w)
    })
    ReplacePreOpW(charw, word)
  }

  def apply(w : Seq[Char], word : List[Int]) : PreOp = ReplacePreOpWWord(w, word)

  def apply(s : String, word : List[Int]) : PreOp = ReplacePreOpW(s.toSeq, word)

  /**
   * PreOp for x = replace(y, e, word) for regex e
   */
  def apply(c : Term, context : PredConj, word : List[Int]) : PreOp =
    ReplacePreOpWRegEx(c, context, word)

  /**
   * PreOp for x = replace(y, e, word) for regex e represented as
   * automaton aut
   */
  def apply(aut : AtomicStateAutomaton, word : List[Int]) : PreOp =
    ReplacePreOpWRegEx(aut, word)
}

/**
 * Companion object for ReplacePreOpWWord, does precomputation of
 * transducer representation of word
 */
object ReplacePreOpWWord {
  def apply(w : Seq[Char], word : List[Int]) = {
    val wtran = buildWordTransducer(w, word)
    new ReplacePreOpWTran(wtran)
  }

  /**
   * Build transducer that identifies first instance of w and replaces it with
   * word
   */
  private def buildWordTransducer(w : Seq[Char], word : List[Int]) : Transducer = {
    val builder = BricsTransducer.getBuilder

    val initState = builder.initialState
    val states = initState::(List.fill(w.size - 1)(builder.getNewState))
    val finstates = List.fill(w.size)(builder.getNewState)
    val copyRest = builder.getNewState
    val nop = OutputOp("", NOP, "")
    // TODO: internal change to word
    // val internal = OutputOp("", Internal, "")
    val outputW = OutputOp("", NOP, word.map(_.toChar))
    val copy = OutputOp("", Plus(0), "")
    val end = w.size - 1

    builder.setAccept(initState, true)
    finstates.foreach(builder.setAccept(_, true))
    builder.setAccept(copyRest, true)

    // recognise word
    // deliberately miss last element
    for (i <- 0 until w.size - 1) {
      builder.addTransition(states(i), (w(i), w(i)), nop, states(i+1))
    }
    builder.addTransition(states(end), (w(end), w(end)), outputW, copyRest)

    // copy rest after first match
    builder.addTransition(copyRest, builder.LabelOps.sigmaLabel, copy, copyRest)

    for (i <- 0 until w.size) {
      val output = OutputOp(w.slice(0, i), Plus(0), "")

      // begin again if mismatch
      val anyLbl = builder.LabelOps.sigmaLabel
      for (lbl <- builder.LabelOps.subtractLetter(w(i), anyLbl))
        builder.addTransition(states(i), lbl, output, states(0))

      // handle word ending in middle of match
      val outop = if (i == w.size -1) outputW else output
      builder.addTransition(states(i), (w(i), w(i)), outop, finstates(i))
    }

    val res = builder.getTransducer
    res
  }
}

/**
 * Companion class for building representation of x = replace(y, e,
 * z) for a regular expression e.
 */
object ReplacePreOpWRegEx {
  /**
   * Build preop from c and context giving regex to be replaced
   */
  def apply(c : Term, context : PredConj, word : List[Int]) : PreOp = {
    val tran = buildTransducer(c, context, word)
    new ReplacePreOpWTran(tran)
  }

  /**
   * Build preop from aut giving regex to be replaced
   */
  def apply(aut : AtomicStateAutomaton, word : List[Int]) : PreOp = {
    val tran = buildTransducer(aut, word)
    new ReplacePreOpWTran(tran)
  }

  /**
   * Builds transducer that identifies leftmost and longest match of
   * regex by rewriting matches to concrete word
   */
  private def buildTransducer(c : Term, context : PredConj, word : List[Int]) : Transducer =
    buildTransducer(BricsAutomaton(c, context), word)

  /**
   * Builds transducer that identifies leftmost and longest match of
   * regex by rewriting matches to concrete word.
   *
   * TODO: currently does not handle empty matches
   */
  private def buildTransducer(aut : AtomicStateAutomaton, word : List[Int]) : Transducer = {
    abstract class Mode
    // not matching
    case object NotMatching extends Mode
    // matching, word read so far could reach any state in frontier
    case class Matching(val frontier : Set[aut.State]) extends Mode
    // last transition finished a match and reached frontier
    case class EndMatch(val frontier : Set[aut.State]) extends Mode
    // copy the rest of the word after first match
    case object CopyRest extends Mode

    val labels = aut.labelEnumerator.enumDisjointLabelsComplete
    val builder = aut.getTransducerBuilder
    val nop = OutputOp("", NOP, "")
    val copy = OutputOp("", Plus(0), "")
    val outputW = OutputOp("", NOP, word.map(_.toChar))

    // TODO: encapsulate this worklist automaton construction

    // states of transducer have current mode and a set of states that
    // should never reach a final state (if they do, a match has been
    // missed)
    val sMap = new MHashMap[aut.State, (Mode, Set[aut.State])]
    val sMapRev = new MHashMap[(Mode, Set[aut.State]), aut.State]

    // states of new transducer to be constructed
    val worklist = new MStack[aut.State]

    def mapState(s : aut.State, q : (Mode, Set[aut.State])) = {
      sMap += (s -> q)
      sMapRev += (q -> s)
    }

    // creates and adds to worklist any new states if needed
    def getState(m : Mode, noreach : Set[aut.State]) : aut.State = {
      sMapRev.getOrElse((m, noreach), {
        val s = builder.getNewState
        mapState(s, (m, noreach))
        val goodNoreach = !noreach.exists(aut.isAccept(_))
        builder.setAccept(s, m match {
          case NotMatching => goodNoreach
          case EndMatch(_) => goodNoreach
          case Matching(_) => false
          case CopyRest => goodNoreach
        })
        if (goodNoreach)
          worklist.push(s)
        s
      })
    }

    val autInit = aut.initialState
    val tranInit = builder.initialState

    mapState(tranInit, (NotMatching, Set.empty[aut.State]))
    builder.setAccept(tranInit, true)
    worklist.push(tranInit)

    while (!worklist.isEmpty) {
      val ts = worklist.pop()
      val (mode, noreach) = sMap(ts)

      mode match {
        case NotMatching => {
          for (lbl <- labels) {
            val initImg = aut.getImage(autInit, lbl)
            val noreachImg = aut.getImage(noreach, lbl)

            val dontMatch = getState(NotMatching, noreachImg ++ initImg)
            builder.addTransition(ts, lbl, copy, dontMatch)

            if (!initImg.isEmpty) {
              val newMatch = getState(Matching(initImg), noreachImg)
              builder.addTransition(ts, lbl, nop, newMatch)
            }

            if (initImg.exists(aut.isAccept(_))) {
              val oneCharMatch = getState(EndMatch(initImg), noreachImg)
              builder.addTransition(ts, lbl, outputW, oneCharMatch)
            }
          }
        }
        case Matching(frontier) => {
          for (lbl <- labels) {
            val frontImg = aut.getImage(frontier, lbl)
            val noreachImg = aut.getImage(noreach, lbl)

            if (!frontImg.isEmpty) {
              val contMatch = getState(Matching(frontImg), noreachImg)
              builder.addTransition(ts, lbl, nop, contMatch)
            }

            if (frontImg.exists(aut.isAccept(_))) {
                val stopMatch = getState(EndMatch(frontImg), noreachImg)
                builder.addTransition(ts, lbl, outputW, stopMatch)
            }
          }
        }
        case EndMatch(frontier) => {
          for (lbl <- labels) {
            val initImg = aut.getImage(autInit, lbl)
            val frontImg = aut.getImage(frontier, lbl)
            val noreachImg = aut.getImage(noreach, lbl)

            val noMatch = getState(CopyRest, initImg ++ frontImg ++ noreachImg)
            builder.addTransition(ts, lbl, copy, noMatch)
          }
        }
        case CopyRest => {
          for (lbl <- labels) {
            val noreachImg = aut.getImage(noreach, lbl)
            val copyRest = getState(CopyRest, noreachImg)
            builder.addTransition(ts, lbl, copy, copyRest)
          }
        }
      }
    }

    val tran = builder.getTransducer
    tran
  }
}

/**
 * Representation of x = replace(y, tran, w) where tran is a
 * transducer that replaces part of the word to be replaced with
 * concrete word w.  Build with companion object ReplacePreOpWWord or
 * ReplacePreOpWTran
 */
class ReplacePreOpWTran(tran : Transducer) extends PreOp {

  override def toString = "replaceW-tran"

  def eval(arguments : Seq[Seq[Int]]) : Option[Seq[Int]] =
    for (s <- tran(arguments(0).map(_.toChar).mkString,
                   arguments(1).map(_.toChar).mkString))
    yield s.toSeq.map(_.toInt)

  def apply(argumentConstraints : Seq[Seq[Automaton]],
            resultConstraint : Automaton)
          : (Iterator[(Seq[Automaton], LinearConstraints)], Seq[Seq[Automaton]]) = {
    val rc : AtomicStateAutomaton = resultConstraint match {
      case resCon : AtomicStateAutomaton => resCon
      case _ => throw new IllegalArgumentException("ReplacePreOpW needs an AtomicStateAutomaton")
    }
    val res = tran.preImage(rc)
    val b = new LinearConstraints
    (Iterator((Seq(res),b)) , argumentConstraints)
  }

//  override def forwardApprox(argumentConstraints : Seq[Seq[Automaton]]) : Automaton = {
//    val yCons = argumentConstraints(0).map(_ match {
//        case saut : AtomicStateAutomaton => saut
//        case _ => throw new IllegalArgumentException("ConcatPreOp.forwardApprox can only approximate AtomicStateAutomata")
//    })
//    val zCons = argumentConstraints(1).map(_ match {
//        case saut : AtomicStateAutomaton => saut
//        case _ => throw new IllegalArgumentException("ConcatPreOp.forwardApprox can only approximate AtomicStateAutomata")
//    })
//    val yProd = ProductAutomaton(yCons)
//    val zProd = ProductAutomaton(zCons)
//
//    PostImageAutomaton(yProd, tran, Some(zProd))
//  }
}


