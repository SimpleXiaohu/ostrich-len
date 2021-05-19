/*
 * This file is part of Sloth, an SMT solver for strings.
 * Copyright (C) 2017-2018  Philipp Ruemmer, Petr Janku
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

package strsolver

import ap.parser._

import scala.collection.mutable.{HashSet => MHashSet}

object StringTheoryTranslator {

  def apply(formula : IFormula,
            globalWordVariables :
            Iterable[IExpression.ConstantTerm],
            stringTheory: StringTheory) : IFormula = {
    ~(new StringTheoryTranslator(~formula, List(),
      globalWordVariables, stringTheory)).newConstraint
  }

}

class StringTheoryTranslator private (constraint : IFormula,
                                      interestingAtoms : Seq[IAtom],
                                      globalWordVariables :
                                      Iterable[IExpression.ConstantTerm],
                                      stringTheory : StringTheory)
  extends ContextAwareVisitor[Unit, IExpression] {

  import IExpression._
  import SMTLIBStringTheory._

  private val toPred = stringTheory.functionPredicateMapping.toMap

  private object SMTLIBPred {
    val reverseMapping =
      (for ((a, b) <- functionPredicateMapping.iterator)
        yield (b, a)).toMap
    def unapply(p : Predicate) : Option[IFunction] =
      reverseMapping get p
  }

  private object StringPred {
    val reverseMapping =
      (for ((a, b) <- toPred.iterator)
        yield (b, a)).toMap
    def unapply(p : Predicate) : Option[IFunction] =
      reverseMapping get p
  }

  //////////////////////////////////////////////////////////////////////////////
  // Detect all character variables/constants in the constraint of the
  // formula

  private val charVariables = new MHashSet[IConstant]

  private object CharVariableDetector extends CollectingVisitor[Unit, Unit] {
    def postVisit(t : IExpression, arg : Unit,
                  subres : Seq[Unit]) : Unit = t match {
      case IExpression.Eq(c : IConstant, d : IConstant) =>
        if ((charVariables contains c) ||
          (charVariables contains d)) {
          charVariables += c
          charVariables += d
        }
      case IAtom(SMTLIBPred(`seq_unit`), Seq(c : IConstant, _)) =>
        charVariables += c
      case IAtom(SMTLIBPred(`seq_cons`), Seq(c : IConstant, _, _)) =>
        charVariables += c
      case IAtom(SMTLIBPred(`seq_rev_cons`), Seq(_, c : IConstant, _)) =>
        charVariables += c
      case IAtom(SMTLIBPred(`seq_head`), Seq(_, c : IConstant)) =>
        charVariables += c
      case IAtom(SMTLIBPred(`seq_last`), Seq(_, c : IConstant)) =>
        charVariables += c
      case IAtom(SMTLIBPred(`seq_nth`), Seq(_, _, c : IConstant)) =>
        charVariables += c
      case IAtom(SMTLIBPred(`re_range`), Seq(c, d, _)) => {
        c match {
          case c : IConstant => charVariables += c
          case _ => // nothing
        }
        d match {
          case d : IConstant => charVariables += d
          case _ => // nothing
        }
      }
      case _ => // nothing
    }
  }

  {
    var oldSize = -1
    while (charVariables.size > oldSize) {
      oldSize = charVariables.size
      CharVariableDetector.visit(constraint, ())
    }
  }

  //////////////////////////////////////////////////////////////////////////////

  private def toTermSeq(s : Seq[IExpression]) =
    (for (e <- s.iterator) yield e.asInstanceOf[ITerm]).toIndexedSeq

  private var constCounter = 0
  private def newConstant = {
    val res = new ConstantTerm("c" + constCounter)
    constCounter = constCounter + 1
    res
  }

  private def polChoice(ctxt : Context[Unit])
                       (pos : => IFormula)(neg : => IFormula) : IFormula =
    if (ctxt.polarity > 0) {
      pos
    } else if (ctxt.polarity < 0) {
      neg
    } else {
      assert(false)
      null
    }

  private def guardedExpr(guard : IFormula, expr : IFormula,
                          ctxt : Context[Unit]) : IFormula =
    polChoice(ctxt) {
      guard & expr
    } {
      guard ==> expr
    }

  //////////////////////////////////////////////////////////////////////////////

  def postVisit(t : IExpression, ctxt : Context[Unit],
                subres : Seq[IExpression]) : IExpression = t match {
    // equations between characters have to be turned into
    // word equations
    case IExpression.Eq(c : IConstant, d)
      if (charVariables contains c) => {
      val a, b = newConstant
      guardedExpr(toPred(stringTheory.wordChar)(c, a) &
        toPred(stringTheory.wordChar)(d, b),
        a === b,
        ctxt)
    }
    case IExpression.Eq(d, c : IConstant)
      if (charVariables contains c) => {
      val a, b = newConstant
      guardedExpr(toPred(stringTheory.wordChar)(c, a) &
        toPred(stringTheory.wordChar)(d, b),
        a === b,
        ctxt)
    }
    case IAtom(SMTLIBPred(`seq_unit`), _) =>
      IAtom(toPred(stringTheory.wordChar), toTermSeq(subres))
    case IAtom(SMTLIBPred(`seq_empty`), _) =>
      IAtom(toPred(stringTheory.wordEps), toTermSeq(subres))
    case IAtom(SMTLIBPred(`seq_concat`), _) =>
      IAtom(toPred(stringTheory.wordCat), toTermSeq(subres))

    case IAtom(SMTLIBPred(`seq_cons`), _) => {
      val Seq(head, tail, res) = toTermSeq(subres)
      val c = newConstant
      guardedExpr(toPred(stringTheory.wordChar)(head, c),
        toPred(stringTheory.wordCat)(c, tail, res),
        ctxt)
    }
    case IAtom(SMTLIBPred(`seq_rev_cons`), _) => {
      val Seq(first, last, res) = toTermSeq(subres)
      val c = newConstant
      guardedExpr(toPred(stringTheory.wordChar)(last, c),
        toPred(stringTheory.wordCat)(first, c, res),
        ctxt)
    }
    case IAtom(SMTLIBPred(`seq_head`), _) => {
      val Seq(str, head) = toTermSeq(subres)
      val a, b, c = newConstant
      guardedExpr(toPred(stringTheory.wordChar)(c, a) &
        toPred(stringTheory.wordCat)(a, b, str),
        c === head,
        ctxt)
    }
    case IAtom(SMTLIBPred(`seq_tail`), _) => {
      val Seq(str, tail) = toTermSeq(subres)
      val a, b, sigma = newConstant
      guardedExpr(toPred(stringTheory.rexSigma)(sigma) &
        stringTheory.member(a, sigma) &
        toPred(stringTheory.wordCat)(a, b, str),
        b === tail,
        ctxt)
    }
    case IAtom(SMTLIBPred(`seq_last`), _) => {
      val Seq(str, last) = toTermSeq(subres)
      val a, b, c = newConstant
      guardedExpr(toPred(stringTheory.wordChar)(c, a) &
        toPred(stringTheory.wordCat)(b, a, str),
        c === last,
        ctxt)
    }
    case IAtom(SMTLIBPred(`seq_first`), _) => {
      val Seq(str, first) = toTermSeq(subres)
      val a, b, sigma = newConstant
      guardedExpr(toPred(stringTheory.rexSigma)(sigma) &
        stringTheory.member(a, sigma) &
        toPred(stringTheory.wordCat)(b, a, str),
        b === first,
        ctxt)
    }

    case IAtom(`seq_prefix_of`, _) => {
      val Seq(shorter, longer) = toTermSeq(subres)
      polChoice(ctxt) {
        toPred(stringTheory.wordCat)(shorter, newConstant, longer)
      } {
        val a, b, aLen, shorterLen, longerLen = newConstant
        (toPred(stringTheory.wordLen)(shorter, shorterLen) &
          toPred(stringTheory.wordLen)(longer, longerLen) &
          toPred(stringTheory.wordCat)(a, b, longer) &
          toPred(stringTheory.wordLen)(a, aLen) &
          (aLen <= shorterLen) & (aLen <= longerLen) &
          ((aLen === shorterLen) | (aLen === longerLen))) ==>
          (a === shorter)
      }
    }
    case IAtom(`seq_suffix_of`, _) => {
      val Seq(shorter, longer) = toTermSeq(subres)
      polChoice(ctxt) {
        toPred(stringTheory.wordCat)(newConstant, shorter, longer)
      } {
        val a, b, aLen, shorterLen, longerLen = newConstant
        (toPred(stringTheory.wordLen)(shorter, shorterLen) &
          toPred(stringTheory.wordLen)(longer, longerLen) &
          toPred(stringTheory.wordCat)(b, a, longer) &
          toPred(stringTheory.wordLen)(a, aLen) &
          (aLen <= shorterLen) & (aLen <= longerLen) &
          ((aLen === shorterLen) | (aLen === longerLen))) ==>
          (a === shorter)
      }
    }
    case IAtom(`seq_subseq_of`, _) => {
      val Seq(shorter, longer) = toTermSeq(subres)
      polChoice(ctxt) {
        val a, b, c = newConstant
        toPred(stringTheory.wordCat)(shorter, b, c) &
          toPred(stringTheory.wordCat)(a, c, longer)
      } {
        // this can be done if "shorter" is a concrete string;
        // for a word variable this looks difficult?
        assert(false)
        null
      }
    }

    case IAtom(SMTLIBPred(`seq_extract`), _) => {
      IAtom(toPred(stringTheory.substring), toTermSeq(subres))
    }
    case IAtom(SMTLIBPred(`seq_indexof`), _) => {
      IAtom(toPred(stringTheory.indexof), toTermSeq(subres))
    }
    case IAtom(`smtparse_contains`, _) => {
      IAtom(stringTheory.str_contains, toTermSeq(subres))
    }
    case IAtom(`smtparse_prefixof`, _) => {
      IAtom(stringTheory.str_prefixof, toTermSeq(subres))
    }    
    case IAtom(SMTLIBPred(`smtparse_at`), _) => {
      IAtom(toPred(stringTheory.str_at), toTermSeq(subres))
    }

    case IAtom(SMTLIBPred(`seq_length`), _) =>
      IAtom(toPred(stringTheory.wordLen), toTermSeq(subres))

    ////////////////////////////////////////////////////////////////////////////

    case IAtom(SMTLIBPred(`re_empty_set`), _) =>
      IAtom(toPred(stringTheory.rexEmpty), toTermSeq(subres))
    case IAtom(SMTLIBPred(`re_full_set`), _) => {
      val Seq(res) = toTermSeq(subres)
      val sigma = newConstant
      guardedExpr(toPred(stringTheory.rexSigma)(sigma),
        toPred(stringTheory.rexStar)(sigma, res),
        ctxt)
    }
    case IAtom(SMTLIBPred(`re_allchar`), _) =>
      IAtom(toPred(stringTheory.rexSigma), toTermSeq(subres))
    case IAtom(SMTLIBPred(`re_concat`), _) =>
      IAtom(toPred(stringTheory.rexCat), toTermSeq(subres))
    case IAtom(SMTLIBPred(`re_empty_seq`), _) =>
      IAtom(toPred(stringTheory.rexEps), toTermSeq(subres))

    case IAtom(SMTLIBPred(`re_star`), _) =>
      IAtom(toPred(stringTheory.rexStar), toTermSeq(subres))
    case IAtom(SMTLIBPred(`re_loop`), _) =>
      assert(false); null
    case IAtom(SMTLIBPred(`re_plus`), _) => {
      val Seq(arg, res) = toTermSeq(subres)
      val a = newConstant
      guardedExpr(toPred(stringTheory.rexStar)(arg, a),
        toPred(stringTheory.rexCat)(arg, a, res),
        ctxt)
    }
    case IAtom(SMTLIBPred(`re_option`), _) => {
      val Seq(arg, res) = toTermSeq(subres)
      val a = newConstant
      guardedExpr(toPred(stringTheory.rexEps)(a),
        toPred(stringTheory.rexUnion)(arg, a, res),
        ctxt)
    }
    case IAtom(SMTLIBPred(`re_range`), _) =>
      IAtom(toPred(stringTheory.rexRange), toTermSeq(subres))

    case IAtom(SMTLIBPred(`re_union`), _) =>
      IAtom(toPred(stringTheory.rexUnion), toTermSeq(subres))
    case IAtom(SMTLIBPred(`re_difference`), _) => {
      val Seq(x, y, res) = toTermSeq(subres)
      val xNeg, xyNeg = newConstant
      guardedExpr(toPred(stringTheory.rexNeg)(x, xNeg) &
        toPred(stringTheory.rexUnion)(xNeg, y, xyNeg),
        toPred(stringTheory.rexNeg)(xyNeg, res),
        ctxt)
    }
    case IAtom(SMTLIBPred(`re_intersect`), _) => {
      val Seq(x, y, res) = toTermSeq(subres)
      val xNeg, yNeg, xyNeg = newConstant
      guardedExpr(toPred(stringTheory.rexNeg)(x, xNeg) &
        toPred(stringTheory.rexNeg)(y, yNeg) &
        toPred(stringTheory.rexUnion)(xNeg, yNeg, xyNeg),
        toPred(stringTheory.rexNeg)(xyNeg, res),
        ctxt)
    }
    case IAtom(SMTLIBPred(`re_complement`), _) =>
      IAtom(toPred(stringTheory.rexNeg), toTermSeq(subres))

    case IAtom(SMTLIBPred(`re_of_pred`), _) =>
      assert(false); null

    case IAtom(`re_member`, _) =>
      IAtom(stringTheory.member, toTermSeq(subres))

    case IAtom(SMTLIBPred(`seq_replace`), _) =>
      IAtom(toPred(stringTheory.replace), toTermSeq(subres))

    case IAtom(SMTLIBPred(`seq_replace_re`), _) =>
      IAtom(toPred(stringTheory.replacere), toTermSeq(subres))

    case IAtom(SMTLIBPred(`seq_replace_all`), _) =>
      IAtom(toPred(stringTheory.replaceall), toTermSeq(subres))

    case IAtom(SMTLIBPred(`seq_replace_all_re`), _) =>
      IAtom(toPred(stringTheory.replaceallre), toTermSeq(subres))

    case IAtom(SMTLIBPred(`seq_reverse`), _) =>
      IAtom(toPred(stringTheory.reverse), toTermSeq(subres))

    case IAtom(SMTLIBPred(f), _)
        if (UserFunctionRegistry.isUserDefinedSMTLIBFun(f.name)) => {

      val strequiv = UserFunctionRegistry.getStringTheoryName(f.name).get
      val strfun = UserFunctionRegistry.getStringTheoryFun(strequiv).get
      IAtom(toPred(strfun), toTermSeq(subres))
    }

    ////////////////////////////////////////////////////////////////////////////

    case t =>
      t update subres
  }

  //////////////////////////////////////////////////////////////////////////////

  private val preConstraint =
    visit(constraint, Context(())).asInstanceOf[IFormula]

  //////////////////////////////////////////////////////////////////////////////
  // Detect word variables that are used in word/regex context

  private val wordVariables  = new MHashSet[IConstant]
  private val regexVariables = new MHashSet[IConstant]

  private object WordVariableDetector extends CollectingVisitor[Unit, Unit] {
    def postVisit(t : IExpression, arg : Unit,
                  subres : Seq[Unit]) : Unit = t match {
      case IExpression.Eq(c : IConstant, d : IConstant) => {
        if ((wordVariables contains c) || (wordVariables contains d)) {
          wordVariables += c
          wordVariables += d
        }
        if ((regexVariables contains c) || (regexVariables contains d)) {
          regexVariables += c
          regexVariables += d
        }
      }

      case IAtom(SMTLIBPred(`re_of_seq`),
      Seq(c : IConstant, _)) =>
        regexVariables += c
      case IAtom(StringPred(stringTheory.replace | stringTheory.replaceall
                            | stringTheory.reverse), args) =>
        for (c <- args) c match {
          case c : IConstant => wordVariables += c
          case _ => // nothing
        }
      case IAtom(StringPred(stringTheory.indexof), args) =>
        for (c <- args) c match {
          case c : IConstant => wordVariables += c
          case _ => 
        }
      case IAtom(StringPred(stringTheory.replaceallre), args) =>
        for (c <- Iterator(args(0), args(2))) c match {
          case c : IConstant => wordVariables += c
          case _ => // nothing
        }
      case IAtom(StringPred(stringTheory.wordLen),
      Seq(c : IConstant, _)) =>
        wordVariables += c
      case IAtom(stringTheory.member,
      Seq(c : IConstant, _)) =>
        wordVariables += c

      case IAtom(StringPred(stringTheory.wordCat),
      Seq(c : IConstant, d : IConstant, e : IConstant)) => {
        if ((wordVariables contains c) ||
          (wordVariables contains d) ||
          (wordVariables contains e)) {
          wordVariables += c
          wordVariables += d
          wordVariables += e
        }
        if (regexVariables contains e) {
          regexVariables += c
          regexVariables += d
        }
      }

      case _ => // nothing
    }
  }

  for (c <- globalWordVariables)
    wordVariables += IConstant(c)

  for (a <- interestingAtoms.iterator;
       c@IConstant(_) <- a.args.iterator)
    wordVariables += c

  {
    var oldWordSize = -1
    var oldRegexSize = -1
    while (wordVariables.size > oldWordSize ||
      regexVariables.size > oldRegexSize) {
      oldWordSize = wordVariables.size
      oldRegexSize = regexVariables.size
      WordVariableDetector.visit(preConstraint, ())
    }
  }

  //////////////////////////////////////////////////////////////////////////////
  // Duplicate/replace word constraints that are used in the context of
  // regular expressions

  private val wordVariableDupl =
    (for (d@IConstant(c) <- wordVariables.iterator;
          if (regexVariables contains d))
      yield (c -> IExpression.i(newConstant))).toMap

  private object WordVariableDuplicator extends CollectingVisitor[Unit, IExpression] {
    def postVisit(t : IExpression, arg : Unit,
                  subres : Seq[IExpression]) : IExpression = t match {
      case IExpression.EqZ(_) => {
        val f = (t update subres).asInstanceOf[IFormula]
        val mapped = ConstantSubstVisitor(f, wordVariableDupl)
        if (mapped == f) f else f & mapped
      }
      case f@IAtom(StringPred(stringTheory.wordEps), Seq(c : IConstant)) =>
        and((if ((wordVariables contains c) ||
          !(regexVariables contains c)) List(f update subres) else List()) ++
          (if (regexVariables contains c)
            List(ConstantSubstVisitor(IAtom(toPred(stringTheory.rexEps),
              toTermSeq(subres)),
              wordVariableDupl))
          else List()))
      case f@IAtom(StringPred(stringTheory.wordChar), Seq(_, c : IConstant)) =>
        and((if ((wordVariables contains c) ||
          !(regexVariables contains c)) List(f update subres) else List()) ++
          (if (regexVariables contains c)
            List(ConstantSubstVisitor(IAtom(toPred(stringTheory.rexChar),
              toTermSeq(subres)),
              wordVariableDupl))
          else List()))
      case f@IAtom(StringPred(stringTheory.wordCat), Seq(_, _, c : IConstant)) =>
        and((if ((wordVariables contains c) ||
          !(regexVariables contains c)) List(f update subres) else List()) ++
          (if (regexVariables contains c)
            List(ConstantSubstVisitor(IAtom(toPred(stringTheory.rexCat),
              toTermSeq(subres)),
              wordVariableDupl))
          else List()))
      case IAtom(SMTLIBPred(`re_of_seq`),
      Seq(IConstant(c), d)) =>
        (wordVariableDupl get c) match {
          case Some(e) => e === d
          case None    => c === d
        }
      case t =>
        t update subres
    }
  }

  val newConstraint =
    WordVariableDuplicator.visit(preConstraint, ()).asInstanceOf[IFormula]

}
