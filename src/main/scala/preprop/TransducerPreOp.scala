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

object TransducerPreOp {
  def apply(t : Transducer) = new TransducerPreOp(t)
}

/**
* Representation of x = T(y)
*/
class TransducerPreOp(t : Transducer) extends PreOp {

  override def toString = "transducer"

  def eval(arguments : Seq[Seq[Int]]) : Option[Seq[Int]] = {
    assert (arguments.size == 1)
    val arg = arguments(0).map(_.toChar).mkString
    for (s <- t(arg)) yield s.toSeq.map(_.toInt)
  }

  def apply(argumentConstraints : Seq[Seq[Automaton]],
            resultConstraint : Automaton)
          : (Iterator[(Seq[Automaton], LinearConstraints)], Seq[Seq[Automaton]]) = {
    val rc : AtomicStateAutomaton = resultConstraint match {
      case resCon : AtomicStateAutomaton => resCon
      case _ => throw new IllegalArgumentException("TransducerPreOp needs an AtomicStateAutomaton")
    }
    val a = new LinearConstraints
    (Iterator((Seq(t.preImage(rc)), a)), List())
  }

//  override def forwardApprox(argumentConstraints : Seq[Seq[Automaton]]) : Automaton = {
//    val cons = argumentConstraints(0).map(_ match {
//        case saut : AtomicStateAutomaton => saut
//        case _ => throw new IllegalArgumentException("ConcatPreOp.forwardApprox can only approximate AtomicStateAutomata")
//    })
//    val prod = ProductAutomaton(cons)
//    PostImageAutomaton(prod, t)
//  }
}
