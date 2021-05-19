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

import ap._
import ap.basetypes.IdealInt
import ap.parser._
import ap.proof.goal.Goal
import ap.proof.theoryPlugins.Plugin
import ap.terfor._
import ap.terfor.conjunctions.Conjunction
import ap.terfor.linearcombination.LinearCombination
import ap.terfor.preds.{Predicate, Atom => PAtom}
import ap.theories._
import ap.util.Seqs

import scala.collection.mutable.{ArrayBuffer, LinkedHashMap, LinkedHashSet, HashMap => MHashMap, HashSet => MHashSet}
object StringTheoryVal  {
  
  val wordEps    = new IFunction("wordEps",    0, true, false)
  val wordCat    = new IFunction("wordCat",    2, true, false)
  val wordChar   = new IFunction("wordChar",   1, true, false)

  val wordLen    = new IFunction("wordLen",    1, true, false)    // length func

  // defined operation
  val wordSlice  = new IFunction("wordSlice",  3, true, false)

  val rexEmpty   = new IFunction("rexEmpty",   0, true, false)
  val rexEps     = new IFunction("rexEps",     0, true, false)
  val rexSigma   = new IFunction("rexSigma",   0, true, false)
  val rexCat     = new IFunction("rexCat",     2, true, false)
  val rexChar    = new IFunction("rexChar",    1, true, false)
  val rexUnion   = new IFunction("rexUnion",   2, true, false)
  val rexStar    = new IFunction("rexStar",    1, true, false)
  val rexNeg     = new IFunction("rexNeg",     1, true, false)
  val rexRange   = new IFunction("rexRange",   2, true, false)

  /// Constraints representing transducers
  val replaceall = new IFunction("replaceall", 3, true, false)  // pattern is a concreteword
  val replaceallre = new IFunction("replaceallre", 3, true, false)  // pattern is a regex
  val replace    = new IFunction("replace",    3, true, false)  // pattern is a concreteword
  val replacere  = new IFunction("replacere",  3, true, false)  // pattern is a regex
  val reverse    = new IFunction("reverse",    1, true, false)
  val wordDiff   = new Predicate("wordDiff",   2)

  // hu zi add -------------------------------------------------------------------
  val substring  = new IFunction("substring",  3, true, false)
  val indexof    = new IFunction("indexof",    3, true, false)
  val str_contains = new Predicate("str_contains",    2)
  val str_prefixof = new Predicate("str_prefixof",    2)
  val str_at     = new IFunction("str_at",     2, true, false)
  // hu zi add -------------------------------------------------------------------

  val member     = new Predicate ("member",    2)
  val functions = List(wordEps, wordCat, wordChar, wordLen, wordSlice,
    rexEmpty, rexEps, rexSigma, rexCat, rexChar,
    rexUnion, rexStar, rexNeg, rexRange, replaceall,
    replaceallre, replace, reverse,
    // hu zi add -------------------------------------------------------------------
    substring, indexof, str_at) ++
    // hu zi add -------------------------------------------------------------------
    UserFunctionRegistry.stringTheoryFuns

  // TODO: have different theory objects for the different solvers
  val iAxioms =
    IBoolLit(true)

  val (functionalPredicatesSeq, preAxioms, preOrder,
  functionPredicateMap) =
    Theory.genAxioms(theoryFunctions = functions,
      theoryAxioms = iAxioms)

  val functionPredicateMapping = functions zip functionalPredicatesSeq
  // huzi modify------------------------
  val order = preOrder extendPred List(member, wordDiff, str_contains)
  val functionalPredicates = functionalPredicatesSeq.toSet
  val predicates = List(member, wordDiff, str_contains, str_prefixof) ++ functionalPredicatesSeq
  private val predFunMap =
    (for ((f, p) <- functionPredicateMap) yield (p, f)).toMap
  object FunPred {
    def unapply(p : Predicate) : Option[IFunction] = predFunMap get p
  }
}

class StringTheory (flags: Flags) extends Theory {

  override def toString = "StringTheory"
  import StringTheoryVal._

  // TODO: use proper sorts for the operations
                                //  name     arity partial relational
                                //    ↓        ↓    ↓     ↓
   val wordEps    = StringTheoryVal.wordEps
   val wordCat    = StringTheoryVal.wordCat
   val wordChar   = StringTheoryVal.wordChar

   val wordLen    = StringTheoryVal.wordLen

//   defined operation
   val wordSlice  = StringTheoryVal.wordSlice

   val rexEmpty   = StringTheoryVal.rexEmpty
   val rexEps     = StringTheoryVal.rexEps
   val rexSigma   = StringTheoryVal.rexSigma
   val rexCat     = StringTheoryVal.rexCat
   val rexChar    = StringTheoryVal.rexChar
   val rexUnion   = StringTheoryVal.rexUnion
   val rexStar    = StringTheoryVal.rexStar
   val rexNeg     = StringTheoryVal.rexNeg
   val rexRange   = StringTheoryVal.rexRange

//  / Constraints representing transducers
   val replaceall = StringTheoryVal.replaceall
   val replaceallre = StringTheoryVal.replaceallre
   val replace    = StringTheoryVal.replace
   val replacere  = StringTheoryVal.replacere
   val reverse    = StringTheoryVal.reverse
   val wordDiff   = StringTheoryVal.wordDiff

   val substring  = StringTheoryVal.substring
   val indexof    = StringTheoryVal.indexof
   val str_contains = StringTheoryVal.str_contains
   val str_prefixof = StringTheoryVal.str_prefixof
   val str_at     = StringTheoryVal.str_at

   val member     = StringTheoryVal.member

  def word(ts : AnyRef*) = {
    import IExpression._
    val it = for (t <- ts.iterator;
                  s <- t match {
                         case t : String =>
                           for (c <- t.iterator) yield wordChar(c.toInt)
                         case t : IdealInt =>
                           Iterator single wordChar(t)
                         case t : Seq[_] =>
                           for (c <- t.iterator) yield c match {
                             case c : IdealInt => wordChar(c)
                             case c : Int      => wordChar(c)
                           }
                         case t : ITerm =>
                           Iterator single t
                         case t : ConstantTerm =>
                           Iterator single i(t)
                       }) yield s
    if (it.hasNext)
      it reduceLeft { (a, b) => wordCat(a, b) }
    else
      wordEps()
  }

  def rex(ts : AnyRef*) : ITerm = {
    import IExpression._
    val it = for (t <- ts.iterator;
                  s <- t match {
                         case t@IFunApp(`rexEmpty`, _) =>
                           return t
                         case IFunApp(`rexEps`, _) =>
                           Iterator.empty
                         case t : ITerm =>
                           Iterator single t
                         case t : ConstantTerm =>
                           Iterator single i(t)
                         case t : String =>
                           for (c <- t.iterator) yield rexChar(c.toInt)
                         case t : IdealInt =>
                           Iterator single rexChar(t)
                         case t : Seq[_] =>
                           for (c <- t.iterator) yield  c match {
                             case c : IdealInt => rexChar(c)
                             case c : Int      => rexChar(c)
                           }
                       }) yield s
    if (it.hasNext)
      it reduceLeft { (a, b) => rexCat(a, b) }
    else
      rexEps()
  }

  def union(ts : AnyRef*) : ITerm = {
    import IExpression._
    val it = for (t <- ts.iterator;
                  te = rex(t);
                  if (te != rexEmpty()))
             yield te
    if (it.hasNext)
      it reduceLeft { (a, b) => rexUnion(a, b) }
    else
      rexEmpty()
  }

  def star(ts : AnyRef*) : ITerm = {
    import IExpression._
    rex(ts : _*) match {
      case t@IFunApp(`rexEps`, _) => t
      case IFunApp(`rexEmpty`, _) => rexEps()
      case t                      => rexStar(t)
    }
  }

  //////////////////////////////////////////////////////////////////////////////

  override val functionalPredicates: Set[Predicate] = StringTheoryVal.functionalPredicates
  override val functionPredicateMapping: Seq[(IFunction, Predicate)] = StringTheoryVal.functionPredicateMapping
  override val functions: Seq[IFunction] = StringTheoryVal.functions
  override val predicates: Seq[Predicate] = StringTheoryVal.predicates
  val totalityAxioms = Conjunction.TRUE

  private val predFunMap =
    (for ((f, p) <- functionPredicateMap) yield (p, f)).toMap
  object FunPred {
    def unapply(p : Predicate) : Option[IFunction] = predFunMap get p
  }

  private val p = functionPredicateMap

  val axioms =
    Conjunction.TRUE

  val predicateMatchConfig : Signature.PredicateMatchConfig = Map()
  val triggerRelevantFunctions : Set[IFunction] = functions.toSet

  //////////////////////////////////////////////////////////////////////////////

  def plugin = Some(new Plugin {

    def asConst(lc : LinearCombination) = {
      assert(lc.size == 1 &&
             lc.leadingCoeff.isOne &&
             lc.leadingTerm.isInstanceOf[ConstantTerm])
      lc.leadingTerm.asInstanceOf[ConstantTerm]
    }

    ////////////////////////////////////////////////////////////////////////////

    // not used
    def generateAxioms(goal : Goal)
          : Option[(Conjunction, Conjunction)] = None

    ////////////////////////////////////////////////////////////////////////////

    // private val afaSolver = new AFASolver
    private val prepropSolver = new PrepropSolver(flags)

    private val modelCache =
      new ap.util.LRUCache[Conjunction,
                           Option[Map[Term, List[Either[Int, Term]]]]](3)

    private def findStringModel(goal : Goal)
                              : Option[Map[Term, List[Either[Int, Term]]]] =
      modelCache(goal.facts) {
                for (m <- prepropSolver.findStringModel(goal)) yield {
                  m mapValues (w => w map (Left(_)))
                }
//            }
      }

    override def handleGoal(goal : Goal)
                       : Seq[Plugin.Action] = goalState(goal) match {

      case Plugin.GoalState.Final => Console.withOut(Console.err) {

        breakCyclicEquations(goal) match {
          case Some(actions) =>
            actions
          case None =>
                findStringModel(goal) match {
                  case Some(model) => List()
                  case None        => List(Plugin.AddFormula(Conjunction.TRUE))
                }

        }
      }

      case _ => List()
    }
  })

  //////////////////////////////////////////////////////////////////////////////

  ////////////////////////////////////////////////////////////////////////
  // check for cyclic word equations, and break those
  // e.g., equations x = yz & y = ax  ->  z = eps & a = eps & y = ax
  // Tarjan's algorithm is used to find all strongly connected components

  private def breakCyclicEquations(goal : Goal)
                    : Option[Seq[Plugin.Action]] = {

      import TerForConvenience._
      implicit val _ = goal.order

      val newAtoms = new ArrayBuffer[PAtom]
      val removedAtoms = new ArrayBuffer[PAtom]

      {
        val wordCatAtoms = goal.facts.predConj positiveLitsWithPred p(wordCat)
        val successors = wordCatAtoms groupBy (_(2))

        val index, lowlink = new MHashMap[LinearCombination, Int]
        val stack = new LinkedHashSet[LinearCombination]
        val component = new MHashSet[LinearCombination]
        val cycle = new LinkedHashMap[LinearCombination, (PAtom, LinearCombination)]

        def connect(v : LinearCombination) : Unit = {
          val vIndex = index.size
          index.put(v, vIndex)
          lowlink.put(v, vIndex)
          stack += v

          for (a <- successors.getOrElse(v, List()).iterator;
               w <- Seqs.doubleIterator(a(0), a(1)))
            (index get w) match {
              case Some(wIndex) =>
                if (stack contains w)
                  lowlink.put(v, lowlink(v) min index(w))
              case None => {
                connect(w)
                lowlink.put(v, lowlink(v) min lowlink(w))
              }
            }

          if (lowlink(v) == vIndex) {
            // found a strongly connected component
            var next = stack.last
            stack remove next
            component += next
            while (next != v) {
              next = stack.last
              stack remove next
              component += next
            }

//              println(component.toList)

            // check whether we can construct a cycle within the
            // component
            var curNode = v
            while (curNode != null && !(cycle contains curNode)) {
              val it = successors.getOrElse(curNode, List()).iterator
              var atom : PAtom = null
              var nextNode : LinearCombination = null
              var sideNode : LinearCombination = null

              while (atom == null && it.hasNext) {
                val a = it.next
                if (component contains a(0)) {
                  atom = a
                  nextNode = a(0)
                  sideNode = a(1)
                } else if (component contains a(1)) {
                  atom = a
                  nextNode = a(1)
                  sideNode = a(0)
                }
              }

              if (atom != null)
                cycle.put(curNode, (atom, sideNode))
              curNode = nextNode
            }

            if (curNode != null) {
              // then we have found a cycle
              var started = false
              for ((v, (a, w)) <- cycle) {
                if (!started && v == curNode) {
                  removedAtoms += a
                  started = true
                }

                if (started)
                  newAtoms += p(wordEps)(List(w))
              }
            }

            component.clear
            cycle.clear
          }
        }

        for (v <- successors.keysIterator)
          if (!(index contains v))
            connect(v)
      }

      ////////////////////////////////////////////////////////////////////////

      if (newAtoms.nonEmpty || removedAtoms.nonEmpty)
        Some(List(Plugin.RemoveFacts(conj(removedAtoms)),
                  Plugin.AddFormula(!conj(newAtoms))))
      else
        None

  }

  //////////////////////////////////////////////////////////////////////////////

  override def isSoundForSat(
         theories : Seq[Theory],
         config : Theory.SatSoundnessConfig.Value) : Boolean =
    theories.size == 1 &&
    (Set(Theory.SatSoundnessConfig.Elementary,
         Theory.SatSoundnessConfig.Existential) contains config)

  case class DecoderData(m : Map[IdealInt, Seq[IdealInt]])
       extends Theory.TheoryDecoderData

  val asSeq = new Theory.Decoder[Seq[IdealInt]] {
    def apply(d : IdealInt)
             (implicit ctxt : Theory.DecoderContext) : Seq[IdealInt] =
      (ctxt getDataFor StringTheory.this) match {
        case DecoderData(m) => m(d)
      }
  }

  val asString = new Theory.Decoder[String] {
    def apply(d : IdealInt)
             (implicit ctxt : Theory.DecoderContext) : String =
      asStringPartial(d).get
  }

  val asStringPartial = new Theory.Decoder[Option[String]] {
    def apply(d : IdealInt)
             (implicit ctxt : Theory.DecoderContext) : Option[String] =
      (ctxt getDataFor StringTheory.this) match {
        case DecoderData(m) =>
          for (s <- m get d)
          yield ("" /: s) { case (res, c) => res + c.intValueSafe.toChar }
      }
  }

  override def generateDecoderData(model : Conjunction)
                                  : Option[Theory.TheoryDecoderData] = {
    val atoms = model.predConj

    val stringMap = new MHashMap[IdealInt, Seq[IdealInt]]

    for (a <- atoms positiveLitsWithPred p(wordEps))
      stringMap.put(a(0).constant, List())

    for (a <- atoms positiveLitsWithPred p(wordChar))
      stringMap.put(a(1).constant, List(a(0).constant))

    var oldMapSize = 0
    while (stringMap.size != oldMapSize) {
      oldMapSize = stringMap.size
      for (a <- atoms positiveLitsWithPred p(wordCat)) {
        for (s0 <- stringMap get a(0).constant;
             s1 <- stringMap get a(1).constant)
          stringMap.put(a(2).constant, s0 ++ s1)
      }
    }

    Some(DecoderData(stringMap.toMap))
  }

  TheoryRegistry register this

  //////////////////////////////////////////////////////////////////////////////

  object NullStream extends java.io.OutputStream {
    def write(b : Int) = {}
  }
}
