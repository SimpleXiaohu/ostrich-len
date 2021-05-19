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

import java.io.{FileWriter, PrintWriter}

import ap.SimpleAPI
import ap.SimpleAPI.ProverStatus
import ap.parser.{Internal2InputAbsy, SymbolCollector}
import ap.terfor._
import ap.util.Seqs
import strsolver.{Flags, IntConstraintStore}

import scala.collection.mutable.{ArrayBuffer, ArrayStack, LinkedHashSet, BitSet => MBitSet, HashMap => MHashMap, HashSet => MHashSet}
import scala.language.postfixOps
import scala.sys.process._

case class TermConstraint(t : Term, aut : Automaton)
object Exploration {

  type ConflictSet = Seq[TermConstraint]

  abstract class ConstraintStore {
    def push : Unit

    def pop : Unit

    /**
     * Add aut to constraintsSet for backjump cut
     */
    def addAut(aut : Automaton) : Unit

    /**
     * Add new automata to the store, return a sequence of term constraints
     * in case the asserted constraints have become inconsistent
     */
    def assertConstraint(aut : Automaton) : Option[ConflictSet]

    /**
     * Return some representation of the asserted constraints
     */
    def getContents : List[Automaton]

    /**
     * Return all constraints that were asserted (without any modifications)
     */
    def getCompleteContents : List[Automaton]

    /**
     * Make sure that the exact length abstraction for the intersection of the
     * stored automata has been pushed to the length prover
     */

    /**
     * Produce an arbitrary word accepted by all the stored constraints
     */
    def getAcceptedWord : Seq[Int]

    /**
     * Produce a word of length <code>len</code> accepted by all the stored
     * constraints
     */
    def getAcceptedWordLen(len : Int) : Seq[Int]
  }

  def lazyExp(funApps : Seq[(PreOp, Seq[Term], Term)],
              intFunApps : Seq[(PreOp, Seq[Term], Term)],
              concreteValues : MHashMap[Term, Seq[Int]],
              flags: Flags,
              initialConstraints : Seq[(Term, Automaton)],
              strictLengths : Boolean) : Exploration =
    new LazyExploration(funApps, intFunApps, concreteValues, flags, initialConstraints,
                        strictLengths)

  private case class FoundModel(model : Map[Term, Seq[Int]]) extends Exception


  def measure[A](op : String)(comp : => A) : A =
//    if (Flags.measuretime)
//     ap.util.Timer.measure(op)(comp)
//    else
       comp
}

////////////////////////////////////////////////////////////////////////////////

/**
 * Depth-first exploration of a conjunction of function applications
 */
abstract class Exploration(val funApps : Seq[(PreOp, Seq[Term], Term)],
                           val intFunApps : Seq[(PreOp, Seq[Term], Term)],
                           val concreteValues : MHashMap[Term, Seq[Int]],
                           val flags: Flags,
                           val initialConstraints : Seq[(Term, Automaton)],
                           strictLengths : Boolean) {

  import Exploration._
//debug----------------
  println
  println("Running preprop solver")

  val notAtomTermSet = new MHashSet[Term]()
  // store int constraints deriving from preimage
  val LCStack = new ArrayStack[LinearConstraints]
  val eachAtomConstraints = new ArrayBuffer[ArrayBuffer[TermConstraint]]()

  private val (allTerms, sortedFunApps)
              : (Set[Term], Seq[(Seq[(PreOp, Seq[Term])], Term)]) = {
    val argTermNum = new MHashMap[Term, Int]    
    for ((_, _, res) <- funApps) {
      argTermNum.put(res, 0)
      notAtomTermSet += res
    }
    for ((t, _) <- initialConstraints)
      argTermNum.put(t, 0)    
    for ((_, args, _) <- funApps; a <- args)
      argTermNum.put(a, argTermNum.getOrElse(a, 0) + 1)  


    var remFunApps = funApps
    val sortedApps = new ArrayBuffer[(Seq[(PreOp, Seq[Term])], Term)]

    while (!remFunApps.isEmpty) {
      val (selectedApps, otherApps) =
        remFunApps partition { case (_, _, res) => argTermNum(res) == 0 }
      remFunApps = otherApps

      for ((_, args, _) <- selectedApps; a <- args)
        argTermNum.put(a, argTermNum.getOrElse(a, 0) - 1)

      assert(!selectedApps.isEmpty)

      val appsPerRes = selectedApps groupBy (_._3)
      // non-repeat non-arg(in other words, res) Terms
      val nonArgTerms = (selectedApps map (_._3)).distinct

      for (t <- nonArgTerms)
        sortedApps +=
          ((for ((op, args, _) <- appsPerRes(t)) yield (op, args), t))
    }

    val intApps = new ArrayBuffer[(Seq[(PreOp, Seq[Term])], Term)]

    for ((op, args, t) <- intFunApps; a <- args){
      argTermNum.put(a, argTermNum.getOrElse(a, 0) + 1)
    }
    for ((op, args, t) <- intFunApps){
      argTermNum.put(t,0)
      intApps += ((Seq((op,args)), t))
      notAtomTermSet += t
    }


    val allFunApps : Seq[(Seq[(PreOp, Seq[Term])], Term)]
      = intApps ++ sortedApps

    (argTermNum.keySet.toSet, allFunApps)
    

  }

  for ((ops, t) <- sortedFunApps)
    if (ops.size > 1) {
      printf("Mutiple definitions found for "+t+"\n")
      printf("unknown\n")
      System.exit(0)
    }

  println("   Considered function applications:")
  for ((apps, res) <- sortedFunApps) {
    println("   " + res + " =")
    for ((op, args) <- apps)
      println("     " + op + "(" + (args mkString ", ") + ")")
  }



  //////////////////////////////////////////////////////////////////////////////

  // check whether we have to add further regex constraints to ensure
  // completeness; otherwise not all pre-images of function applications might
  // be considered
  
  val allInitialConstraints = {
    val term2Index =
      (for (((_, t), n) <- sortedFunApps.iterator.zipWithIndex)
       yield (t -> n)).toMap

    val coveredTerms = new MBitSet
    for ((t, _) <- initialConstraints)
      for (ind <- term2Index get t)
        coveredTerms += ind

    val additionalConstraints = new ArrayBuffer[(Term, BricsAutomaton)]

    // check whether any of the terms have concrete definitions
    for (n <- 0 until sortedFunApps.size;
         if {
           if (!(coveredTerms contains n)) {
             coveredTerms += n
             additionalConstraints +=
               ((sortedFunApps(n)._2, BricsAutomaton.makeAnyString()))
           }
           true
         };
         (_, args) <- sortedFunApps(n)._1;
         arg <- args)
      for (ind <- term2Index get arg)
        coveredTerms += ind

    initialConstraints ++ additionalConstraints
  }

  ///////////////////////////////////////////////////////////////////////////////
  // get product of all initial automata
  val autSet = allInitialConstraints.groupBy(_._1).map(_._2)
    .map(_.map(_._2))
    .map(_.map(_.asInstanceOf[BricsAutomaton]))
  val termSet = allInitialConstraints.groupBy(_._1).map(_._1)
  val productInitialConstraints = termSet zip autSet.map(BricsAutomaton.productNormally(_))
  val debug = productInitialConstraints
  //////////////////////////////////////////////////////////////////////////////


  private val constraintStores = new MHashMap[Term, ConstraintStore]
  private val atomConstraints = new AtomConstraints
  val storeLC = new StoreLC
  def findModel : Option[Map[Term, Seq[Int]]] = {
      for (t <- allTerms)
        constraintStores.put(t, newStore(t))

      // check whether initialConstraints are consistent
      for ((t, aut) <- productInitialConstraints) {
        // add term's aut to constraints , TODO： watch this ---------------------------------------------
        constraintStores(t).assertConstraint(aut) match {
          case Some(_) => return None
          case None    => // nothing
        }
        // Initially, add allInitialConstraints to AtomConstraints
        atomConstraints.addConstraints(TermConstraint(t,aut))
      }

    val funAppList =
      (for ((apps, res) <- sortedFunApps;
            (op, args) <- apps)
       yield (op, args, res)).toList


    try {
      if(flags.useParikh)
        dfExploreComplete(funAppList)
      else{
        dfExploreHeuri(funAppList)
      }
      if(flags.nuxmvTimeout){
        println("unknown")
        System.exit(0)
      }
      None
    } catch {
      case FoundModel(model) => Some(model)
    }
  }


  private def printIntConstraint(atomAuts : Seq[Seq[BricsAutomaton]]):Unit = {
    val out = new PrintWriter(new FileWriter(flags.tmpFileName))
    val constantTermSet = new MHashSet[ConstantTerm]()
    constantTermSet ++= SymbolCollector constantsSorted Internal2InputAbsy(IntConstraintStore())
    for(i <- 0 to LCStack.size-1) {
      val preOpIntFormula = LCStack(i)
      preOpIntFormula().foreach{
        case a => constantTermSet ++= SymbolCollector.constantsSorted((a))
      }
    }
    constantTermSet ++= SymbolCollector constantsSorted storeLC()

    val notDeclare = getNotDeclare(atomAuts)
    out.print("declare: ")
    out.println((constantTermSet filterNot notDeclare).map(_.toString).toSet)
    out.println("#intconstraints:")
    // input int
    if(IntConstraintStore().toString == "true")
      out.print(IntConstraintStore().toString.toUpperCase())
    else
      out.print(IntConstraintStore())

    // preop int
    for(i <- 0 to LCStack.size-1) {
      val preOpIntFormula = LCStack(i)
      preOpIntFormula().foreach{
        case f => out.print(" & "+f)
      }
    }
    //derived int
    out.println(" & "+storeLC().toString.toUpperCase()+" & TRUE")
    storeLC.clean()

    out.close()
  }
  private def getAtomAuts(cons : ArrayBuffer[TermConstraint]) : Seq[ArrayBuffer[BricsAutomaton]] = {
    cons.groupBy{case TermConstraint(aTerm, _) => aTerm}.map(_._2)
      .map{case a => a.map{case TermConstraint(_,aut)=>AtomicStateAutomatonAdapter.intern(aut).asInstanceOf[BricsAutomaton]}}
      .toSeq
  }
  /**
   * get optimal auts
   */
  def  getOptimalAuts(oldAuts: Seq[BricsAutomaton]) : List[BricsAutomaton]
  private  def printAtomAuts(atomAuts : Seq[Seq[BricsAutomaton]]) = {
    val out = new PrintWriter(new FileWriter(flags.tmpFileName, true))
    atomAuts.foreach{
      case auts => {
        out.println("$")
        auts.foreach{
          case aut => {
            val statesIndex = aut.states.zipWithIndex.toMap
            out.println("#automata:")
            out.println("#states: " + aut.states.size)
            out.println("#init: " + statesIndex(aut.initialState))
            out.print("#final: ")
            aut.acceptingStates.foreach { case state => out.print(statesIndex(state) + ",") }
            out.println()
            out.println("#transitons: ")
            aut.etaMap.foreach {
              case ((from, (charmin, charmax), to), vector) =>
                {
                  val min = charmin
                  var max = charmax
                  if (charmax == 65535)
                    // ascii char max is 127
                    max = 127
                  out.println(statesIndex(from) + ";" + statesIndex(to) + ";" + min.toInt + ";" + max.toInt + ";" + vector)
                }
            }
            out.print("#register: ")
            aut.registers.foreach { case r => out.print(r + ";") }
            out.println()
            out.println("#end")
          }
        }
        out.println("$")
      }
    }
    out.close()
  }
// get product of all (term, aut) in cons, result is final producted bricsAutomaton of each term.  
  private def getProductAuts(cons : ArrayBuffer[TermConstraint]) : Seq[BricsAutomaton] = {
    // not test, maybe wrong
    val atomSeq : Seq[ArrayBuffer[BricsAutomaton]] =
      cons.groupBy{case TermConstraint(aTerm, _) => aTerm}.map(_._2)
        .map{case a => a.map{case TermConstraint(_,aut)=>AtomicStateAutomatonAdapter.intern(aut).asInstanceOf[BricsAutomaton]}}
        .toSeq
    val res = atomSeq.map(BricsAutomaton.product(_))
    res
  }

  // get parikhImage of each bricsAutomaton in auts
  private def getAutsParikhImage(auts : Seq[BricsAutomaton]) : List[Formula] = {
    (auts.map{aut => aut.parikhImage}).toList
  }

  private def getNotDeclare(seq: Seq[Seq[BricsAutomaton]]) = {
    val res = new MHashSet[ConstantTerm]()
    seq.foreach(
      _.foreach{
        case aut => res ++=
          aut.registers.map(SymbolCollector constants(_) head)

      }
    )
    res
  }

  def nuxmvCompute() : Int = {
    var res = 0
    var str = ""
    if(flags.strategy == "-I")
      str = Seq("./a", flags.strategy, flags.tmpFileName, flags.nuxmvTime, "0") .!!
    else 
      str = Seq("./a", flags.strategy, flags.tmpFileName, flags.nuxmvTime,flags.windowSize) .!!
    print(str)
    if(!str.endsWith("\n0\n"))
      // timeout
      res = 2
    else
    if (str.contains("is false"))
      // sat
      res = 1
    else
      // unsat
      res = 0
    res
  }
  /**
   * input topological funclist, return the conflict set, or throw FindModel
   * 
   */
  private def dfExploreHeuri(apps : List[(PreOp, Seq[Term], Term)]
                        )
                      : ConflictSet = apps match {

    case List() => {
      // // we are finished and just have to construct a model
      // if findModel, throw FindModel, else return List() stand for Unknow
      val model = new MHashMap[Term, Seq[Int]]
      if(!strictLengths)      
        // there is no int constraints
        throw FoundModel(model.toMap)

      val tmpBuffer = atomConstraints.constraints.clone()

      // delet term which is not atom term.
      // e.g. x1 = y1·z1;  x2 = y2·z2   x1 and x2 is not atom term
      atomConstraints.constraints.foreach{
        case TermConstraint(term, a) => {
          if(notAtomTermSet(term)){
            tmpBuffer -= TermConstraint(term, a)
          }
        }
      }

      val atomAuts = getAtomAuts(tmpBuffer)
      val optAuts = atomAuts.map(getOptimalAuts(_))
      measure("println Int constraints") {printIntConstraint(optAuts)}
      measure("println atomAuts"){printAtomAuts(optAuts)}
      println("nuxmv compute")
      measure("nuxmv"){nuxmvCompute()} match {
        // unsat
        case 0 => List()
        // satp
        case 1 => throw FoundModel(model.toMap)
        // timeout
        case 2 => {
          flags.nuxmvTimeout = true
          List()
        }
      }
    }
    case (op, args, res) :: otherApps =>
    dfExploreHeuriOp(op, args, res, constraintStores(res).getContents,
      otherApps)
  }

  private def dfExploreHeuriOp(op : PreOp,
                          args : Seq[Term],
                          res : Term,
                          resConstraints : List[Automaton],
                          nextApps : List[(PreOp, Seq[Term], Term)]
                          )
                        : ConflictSet = resConstraints match {
    case List() =>
      dfExploreHeuri(nextApps)

    case resAut :: otherAuts => {
      ap.util.Timeout.check

      var notConflict = false
      val argConstraints =
        for (a <- args) yield constraintStores(a).getCompleteContents
      val collectedConflicts = new LinkedHashSet[TermConstraint]

      val (newConstraintsWithLinear, argDependencies) =
         op(argConstraints, resAut)

      while (newConstraintsWithLinear.hasNext) {

        val (argCS, lC) = newConstraintsWithLinear.next

        atomConstraints.push
        LCStack push lC
        for (a <- args){
          constraintStores(a).push
        }


        try {
          val newConstraints = new MHashSet[TermConstraint]

          var consistent = true
          for ((a, aut) <- args zip argCS)
            if (consistent) {
              newConstraints += TermConstraint(a, aut)
              atomConstraints.addConstraints(TermConstraint(a,aut))
               constraintStores(a).assertConstraint(aut) match {
                 case Some(conflict) => {
                   consistent = false
                   assert(!Seqs.disjointSeq(newConstraints, conflict))
                   collectedConflicts ++=
                     (conflict.iterator filterNot newConstraints)
                 }
                 case None => // nothing
               }
            }



          if (consistent) {
            val conflict = dfExploreHeuriOp(op, args, res, otherAuts, nextApps)
            // huzi add
            if(conflict.isEmpty)
              notConflict = true
          }
        } finally {
          for (a <- args) {
            constraintStores(a).pop
          }
          atomConstraints.pop
          LCStack.pop
        }
      }
      // while end--------------------------------------------------------------------------------------------------------


      if(!notConflict) {
        collectedConflicts += TermConstraint(res, resAut)

        collectedConflicts ++=
          (for ((t, auts) <- args.iterator zip argDependencies.iterator;
                aut <- auts.iterator)
            yield TermConstraint(t, aut))
      }
      collectedConflicts.toSeq
    }
  }

  private def dfExploreComplete(apps : List[(PreOp, Seq[Term], Term)]
                               )
  : ConflictSet = apps match {

    case List() => {
      // // we are finished and just have to construct a model
      // if findModel, throw FindModel, else return List() stand for Unknow
      val model = new MHashMap[Term, Seq[Int]]
      if(!strictLengths)
      // there is no int constraints
        throw FoundModel(model.toMap)

      val tmpBuffer = atomConstraints.constraints.clone()

      // delet term which is not atom term.
      // e.g. x1 = y1·z1;  x2 = y2·z2   x1 and x2 is not atom term
      atomConstraints.constraints.foreach{
        case TermConstraint(term, a) => {
          if(notAtomTermSet(term)){
            tmpBuffer -= TermConstraint(term, a)
          }
        }
      }


      SimpleAPI.withProver{ p=>
        import p._
        val constantTermSet = new MHashSet[ConstantTerm]()

        // the input int constraints
        val inputIntFormula = IntConstraintStore()
        constantTermSet ++= SymbolCollector constantsSorted Internal2InputAbsy(inputIntFormula)
        addAssertion(inputIntFormula)

        // the preop int constraints
        for(i <- 0 to LCStack.size-1) {
          val preOpIntFormula = LCStack(i)
          preOpIntFormula().foreach{
            case a => constantTermSet ++= SymbolCollector.constantsSorted((a))
          }
          preOpIntFormula().foreach(addAssertion(_))
        }

        // the derived int constraints, e.g from substr and lenth relation
        constantTermSet ++= SymbolCollector constantsSorted storeLC()
        addAssertion(storeLC())
        println("product all atom automaton")
        val finalCons = getProductAuts(tmpBuffer)
        println("begin to compute parikh image")
        val parikhIntFormula = getAutsParikhImage(finalCons).map(Internal2InputAbsy(_))
        parikhIntFormula.foreach{case formula => constantTermSet ++= SymbolCollector.constantsSorted(formula)}
        parikhIntFormula.foreach(addAssertion(_))
        println("parikh image compute finished")
        addConstantsRaw(constantTermSet)
        println(???)
        ??? match {
          case ProverStatus.Sat => throw FoundModel(model.toMap)
          // return List() to stand for Unknow
          case ProverStatus.Unsat => return  List()
        }

      }
    }
    case (op, args, res) :: otherApps =>
      dfExploreCompleteOp(op, args, res, constraintStores(res).getContents,
        otherApps)
  }

  private def dfExploreCompleteOp(op : PreOp,
                                  args : Seq[Term],
                                  res : Term,
                                  resConstraints : List[Automaton],
                                  nextApps : List[(PreOp, Seq[Term], Term)]
                                 )
  : ConflictSet = resConstraints match {
    case List() =>
      dfExploreComplete(nextApps)

    case resAut :: otherAuts => {
      ap.util.Timeout.check

      var notConflict = false
      val argConstraints =
        for (a <- args) yield constraintStores(a).getCompleteContents
      val collectedConflicts = new LinkedHashSet[TermConstraint]

      // huzi modify
      val (newConstraintsWithLinear, argDependencies) =
        measure("pre-op") { op(argConstraints, resAut) }

      while (measure("pre-op hasNext") {newConstraintsWithLinear.hasNext}) {

        val (argCS, lC) = newConstraintsWithLinear.next

        atomConstraints.push
        LCStack push lC
        for (a <- args){
          constraintStores(a).push
        }


        try {
          val newConstraints = new MHashSet[TermConstraint]

          var consistent = true
          for ((a, aut) <- args zip argCS)
            if (consistent) {
              newConstraints += TermConstraint(a, aut)
              atomConstraints.addConstraints(TermConstraint(a,aut))

              constraintStores(a).assertConstraint(aut) match {
                case Some(conflict) => {
                  consistent = false
                  //println("assertConstraint false!!!!!!!!!!!!!")
                  assert(!Seqs.disjointSeq(newConstraints, conflict))
                  collectedConflicts ++=
                    (conflict.iterator filterNot newConstraints)
                }
                case None => // nothing
              }
            }



          if (consistent) {
            val conflict = dfExploreCompleteOp(op, args, res, otherAuts, nextApps)
            if(conflict.isEmpty)
              notConflict = true
            else if (Seqs.disjointSeq(newConstraints, conflict)) {
              // we can jump back, because the found conflict does not depend
              // on the considered function application
              println("backjump " + (conflict map { case TermConstraint(t, aut) => (t, aut.hashCode) }))
              return conflict
            }
            collectedConflicts ++= (conflict.iterator filterNot newConstraints)
          }
        } finally {
          for (a <- args) {
            constraintStores(a).pop
          }
          atomConstraints.pop
          LCStack.pop
        }
      }
      // while end--------------------------------------------------------------------------------------------------------


      if(!notConflict) {
        collectedConflicts += TermConstraint(res, resAut)

        collectedConflicts ++=
          (for ((t, auts) <- args.iterator zip argDependencies.iterator;
                aut <- auts.iterator)
            yield TermConstraint(t, aut))
      }
      collectedConflicts.toSeq
    }
  }

  protected def newStore(t : Term) : ConstraintStore


}




/**
 * Version of exploration that keeps automata separate and avoids computation
 * of products. No caching yet
 */
class LazyExploration(_funApps : Seq[(PreOp, Seq[Term], Term)],
                      // huzi add 
                      _intFunApps : Seq[(PreOp, Seq[Term], Term)],
                      _concreteValues : MHashMap[Term, Seq[Int]],
                      _flags : Flags,
                      // huzi add
                      _initialConstraints : Seq[(Term, Automaton)],
                      _strictLengths : Boolean)
      extends Exploration(_funApps, _intFunApps, _concreteValues, _flags, _initialConstraints,
                           _strictLengths) {
  import Exploration._

  // protected val needCompleteContentsForConflicts : Boolean = false

  private val stores = new ArrayBuffer[Store]

  // Combinations of automata that are known to have empty intersection
  private val inconsistentAutomata = new ArrayBuffer[Seq[Automaton]]

  private def addIncAutomata(auts : Seq[Automaton]) : Unit = {
    // if find a unsat core, store it to inconsistentAutomata
    inconsistentAutomata += auts
//    println("Watching " + inconsistentAutomata.size + " sets of inconsistent automata")
    val ind = inconsistentAutomata.size - 1
    for (s <- stores) {
      // if find a unsat core, store it's index to every store's watchAutomata
      val r = s.watchAutomata(auts, ind)
      assert(r)
    }
  }

  protected def newStore(t : Term) : ConstraintStore = new Store(t)
  override def getOptimalAuts(oldAuts: Seq[BricsAutomaton]) : List[BricsAutomaton] = {
    val res = MHashSet[BricsAutomaton]()
    val normalAuts = oldAuts.filter(BricsAutomaton.isNormalAut(_))
    val lenAuts = oldAuts.filter(BricsAutomaton.isLenAut(_))
    val productAuts = normalAuts++lenAuts
    if(productAuts.size > 0) res += BricsAutomaton.lenAutsProduct(productAuts, storeLC)
    val otherAuts = oldAuts.filter(BricsAutomaton.isOtherAut(_))
    res ++= otherAuts
    res.toList
  }
  private class Store(t : Term) extends ConstraintStore {
    //  constraints about t, stored by ArrayBuffer
    private val constraints = new ArrayBuffer[Automaton]
    //  constraints about t, stored by MHashSet
    private val constraintSet = new MHashSet[Automaton]
    // store constraints.size, for push and pop operation 
    private val constraintStack = new ArrayStack[Int]

    // Map from watched automata to the indexes of
    // <code>inconsistentAutomata</code> that is watched
    private val watchedAutomata = new MHashMap[Automaton, List[Int]]

    // huzi add: 
    def addAut(aut : Automaton) = {
      constraints += aut
      constraintSet += aut
    }
    private val productAut = new ArrayBuffer[Automaton]()
    private val productAutStack = new ArrayStack[Int]()
    // initial
    productAut += BricsAutomaton.makeAnyString()

    // Add a new entry to <code>watchedAutomata</code>; return
    // <code>false</code> in case the set of new automata is a subset of the
    // asserted constraints
    def watchAutomata(auts : Seq[Automaton], ind : Int) : Boolean =
      // TODO: we should randomise at this point! 
      // NOW : choose the first automata not in constraintSet, and add it to 
      // watchedAutomata, or just return false.
      (auts find { a => !(constraintSet contains a) }) match {
        case Some(aut) => {
          watchedAutomata.put(aut,
                              ind :: watchedAutomata.getOrElse(aut, List()))
          true
        }
        case None =>
          false
      }

    def push : Unit = {
      constraintStack push constraints.size
      productAutStack push productAut.size
    }

    def pop : Unit = {
      val oldSize = constraintStack.pop
      while (constraints.size > oldSize) {
        constraintSet -= constraints.last
        constraints reduceToSize (constraints.size - 1)
      }

      val oldSize2 = productAutStack.pop
      while (productAut.size > oldSize2) {
        productAut reduceToSize (productAut.size - 1)
      }
    }

    def assertConstraint(aut : Automaton) : Option[ConflictSet] =
      if (constraintSet contains aut) {
        None
      } else {
        // huzi : the logic below maybe wrong!
        var potentialConflicts =
          (watchedAutomata get aut) match {
            case Some(incAuts) => {
              // need to find new watched automata for the found conflicts
              watchedAutomata -= aut
              incAuts
            }
            case None =>
              List()
          }

        while (!potentialConflicts.isEmpty) {
          val autInd = potentialConflicts.head

          if (!watchAutomata(inconsistentAutomata(autInd), autInd)) {
            // constraints have become inconsistent!

            watchedAutomata.put(aut, potentialConflicts)
            println("Stored conflict applies!")
            return Some(for (a <- inconsistentAutomata(autInd).toList)
                        yield TermConstraint(t, a))
          }

          potentialConflicts = potentialConflicts.tail
        }

        // find inconsistent constraints
         measure("AutomataUtils.findUnsatCore") { AutomataUtils.findUnsatCore(constraints, productAut.last,aut) } match {
          case Some(core) => {
            println("find unsat core rrrrrrrrrrrrrrrrrrrrrrrrrr")
            addIncAutomata(core)
            Some(for (a <- core.toList) yield TermConstraint(t, a))
          }
          case None => {
            constraints += aut
            constraintSet += aut
            productAut += productAut.last & aut
            None
          }
        }
      }



    def getContents : List[Automaton] = {
      val bricsAuts = constraints.map(AtomicStateAutomatonAdapter.intern(_).asInstanceOf[BricsAutomaton])
      getOptimalAuts(bricsAuts)
    }

    def getCompleteContents : List[Automaton] =
      constraints.toList

    private def intersection : Automaton = constraints reduceLeft (_ & _)


    def getAcceptedWord : Seq[Int] =
      constraints match {
        case Seq() => List()
        case auts  => intersection.getAcceptedWord.get
      }

    def getAcceptedWordLen(len : Int) : Seq[Int] =
      constraints match {
        case Seq() => for (_ <- 0 until len) yield 0
        case auts  => AutomataUtils.findAcceptedWord(auts, len).get
      }
  }

}


class AtomConstraints {

  val constraints = new ArrayBuffer[TermConstraint]
  val constraintsStack = new ArrayStack[Int]

  def push : Unit = constraintsStack push constraints.size
  def pop : Unit = {
    val oldSize = constraintsStack.pop
    constraints reduceToSize oldSize
  }
  def addConstraints(cons : TermConstraint){
    constraints += cons 
  }
  def deleConstraints(cons : TermConstraint){
    constraints -= cons
  }
}