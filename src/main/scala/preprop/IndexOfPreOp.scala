package strsolver.preprop
import ap.parser.{ITerm, Internal2InputAbsy}
import ap.terfor.Term
import ap.terfor.linearcombination.LinearCombination

import scala.collection.mutable.{ArrayBuffer, SortedSet}

// i = indexof(x,u,j),
object IndexOfPreOp {
  def apply(u : List[Char] , internali : Term, internalj : Term) : PreOp = {
    new IndexOfPreOp(u,internali,internalj)
  }
}

class IndexOfPreOp(u : List[Char], internali : Term, internalj : Term) extends PreOp{

  /**
   * Remove given characters from Sigma label. E.g. [Char.Min, Char.Max] - (5,7) is
   * [Char.Min,4],[6],[8,Char.Max].
   * assume char in s is sored
   */
  val i = Internal2InputAbsy(internali)
  val j = Internal2InputAbsy(internalj)


  def getSpecialPre(resAut: Automaton, i: ITerm, j: ITerm, u: List[Char]) : Iterator[(Seq[Automaton], LinearConstraints)] = {
    var resList : List[(Seq[Automaton], LinearConstraints)]= List()
    // i == -1
    val res_1 = !BricsAutomaton.concat(List(
      BricsAutomaton.makeAnyString,BricsAutomaton.fromString(u.mkString), BricsAutomaton.makeAnyString))
    val a_1 = new LinearConstraints
    a_1.addFormula(i === -1)
    resList = resList :+ ((Seq(res_1), a_1))
    if( i.toString == "-1"){
      return  resList.toIterator
    }

    // i != -1
    val patternLen = u.size
    val a_2= new LinearConstraints
    val builder = new BricsAutomatonBuilder
    val Sigma = builder.LabelOps.sigmaLabel
    val subtract0 = builder.LabelOps.subtractLetter(u(0), Sigma)
    val next = KMP.get_next(u)
    val states = List.fill(patternLen + 1)(builder.getNewState)
    builder.setInitialState(states(0))
    builder.setAccept(states(patternLen), true)

    //
    // add transition
    subtract0.foreach(
        builder.addTransition(states(0), _, states(0), List(1))
    )

    for (i <- 0 to patternLen - 1) {
      builder.addTransition(states(i), (u(i), u(i)), states(i + 1), List(0))
      var j = next(i)
      val seenSet = SortedSet[Char]()
      seenSet += u(i)
      // add transition based on the "next" set in KMP
      while (j != -1) {
        if (!seenSet(u(j))) {
          builder.addTransition(states(i), (u(j), u(j)), states(j + 1), List(i - j))
          seenSet += u(j)
        }
        j = next(j)
      }
      // except for char in seenSet, other char will jump to state(0)
      val otherLable = Util.subtractLettersSigma(seenSet)
      otherLable.foreach(
          builder.addTransition(states(i), _, states(0), List(i + 1))
      )

      // match char after the mathced pattern
      builder.addTransition(states(patternLen), Sigma, states(patternLen), List(0))
    }
    val res = builder.getAutomaton
    res.addNewRegister(1) // i
    a_2.addFormula( i >= 0)
    a_2.addFormula( i === res.registers(0))
    res.addEtaMaps(builder.etaMap)
    resList = resList :+ ((Seq(res),a_2))
    resList.toIterator
  }

  def apply(argumentConstraints : Seq[Seq[Automaton]],
  resultConstraint : Automaton)
    : (Iterator[(Seq[Automaton], LinearConstraints)], Seq[Seq[Automaton]]) = {
    //construct automaton based on KMP algorithm
    var resList :List[(Seq[Automaton], LinearConstraints)] = List()
    val patternLen = u.size
    val builder_1 = new BricsAutomatonBuilder
    val Sigma = builder_1.LabelOps.sigmaLabel
    val subtract0 = builder_1.LabelOps.subtractLetter(u(0), Sigma)
    val next = KMP.get_next(u)
    if( j.toString == "0" ){
      println("handle spacial case")
      val res = getSpecialPre(resultConstraint, i, j, u)
      return (res, argumentConstraints)
    }
    // i == -1
    val a_1 = new LinearConstraints
    val initState_1 = builder_1.getNewState
    builder_1.setInitialState(initState_1)
    val states_1 = (List.fill(patternLen + 1)(builder_1.getNewState)) :+ initState_1
    // except for states(patternLen) which standing for matching pattern successfully,
    // the other states set accepts
    for(i <- 0 to patternLen-1)
      builder_1.setAccept(states_1(i), true)
    builder_1.setAccept(initState_1, true)


    builder_1.addTransition(initState_1, Sigma, initState_1, List(1))
    builder_1.addTransition(initState_1, (u(0), u(0)), states_1(1), List(0))
    subtract0.foreach(
    builder_1.addTransition(states_1(0), _, states_1(0), List(0))
    )
    subtract0.foreach(
    builder_1.addTransition(initState_1, _, states_1(0), List(0))
    )

    for (i <- 0 to patternLen - 1) {
        builder_1.addTransition(states_1(i), (u(i), u(i)), states_1(i + 1), List(0))
        var j = next(i)
        val seenSet = SortedSet[Char]()
        seenSet += u(i)
        while (j != -1) {
          if (!seenSet(u(j))) {
            builder_1.addTransition(states_1(i), (u(j), u(j)), states_1(j + 1), List(0))
            seenSet += u(j)
          }
          j = next(j)
        }
        val otherLable = Util.subtractLettersSigma(seenSet)
        otherLable.foreach(
        builder_1.addTransition(states_1(i), _, states_1(0), List(0))
        )

        builder_1.addTransition(states_1(patternLen), Sigma, states_1(patternLen), List(0))
      }
    val res_1 = builder_1.getAutomaton
    res_1.addNewRegister(1) // (j)
    res_1.addEtaMaps(builder_1.etaMap)
    //   registers(0) <= j, not registers(0) == j
    //   for situation len(x) < j
    a_1.addFormula(res_1.registers(0) <= j)
    a_1.addFormula(j >= 0)    // cvc4 semantics
    // i = -1
    a_1.addFormula(i === -1)
    resList =  resList :+ (Seq(res_1), a_1)
    if(internali == LinearCombination.MINUS_ONE)
      return  (resList.toIterator, argumentConstraints)


    // i!=-1
    val a = new LinearConstraints
    val builder = new BricsAutomatonBuilder
    val initState = builder.getNewState
    builder.setInitialState(initState)
    val states = (List.fill(patternLen + 1)(builder.getNewState)) :+ initState
    builder.setAccept(states(patternLen), true)

    //
    // add transition
    builder.addTransition(initState, Sigma, initState, List(1, 1))
    builder.addTransition(initState, (u(0), u(0)), states(1), List(0, 0))
    subtract0.foreach(
    builder.addTransition(states(0), _, states(0), List(0, 1))
    )
    subtract0.foreach(
    builder.addTransition(initState, _, states(0), List(0, 1))
    )

      for (i <- 0 to patternLen - 1) {
        builder.addTransition(states(i), (u(i), u(i)), states(i + 1), List(0, 0))
        var j = next(i)
        val seenSet = SortedSet[Char]()
        seenSet += u(i)
        // add transition based on the "next" set in KMP
        while (j != -1) {
          if (!seenSet(u(j))) {
            builder.addTransition(states(i), (u(j), u(j)), states(j + 1), List(0, i - j))
            seenSet += u(j)
          }
          j = next(j)
        }
        // except for char in seenSet, other char will jump to state(0)
        val otherLable = Util.subtractLettersSigma(seenSet)
        otherLable.foreach(
        builder.addTransition(states(i), _, states(0), List(0, i + 1))
        )

        // match char after the mathced pattern
        builder.addTransition(states(patternLen), Sigma, states(patternLen), List(0, 0))
      }
      val res = builder.getAutomaton
      res.addNewRegister(2) // (j, r)
      res.addEtaMaps(builder.etaMap)

      a.addFormula(res.registers(0) === j)
      a.addFormula(j >= 0)    // cvc4 semantics
      a.addFormula(res.registers(1) === i)
      a.addFormula(i >= 0)
      resList = resList :+ (Seq(res),a)

      (resList.toIterator, argumentConstraints)
//    (Iterator((Seq(res),a)), argumentConstraints)
  }
    
    def eval(arguments : Seq[Seq[Int]]) : Option[Seq[Int]] = 
      // it is inaccurate, if we want to do some forward optimizition, we need finish this func
      Some(arguments(0))
  override def toString = "indexof{"+u.mkString+","+j.toString+"}"
}

// KMP algorithm
object KMP{
  def get_next(s : List[Char]) : ArrayBuffer[Int] = {
    val patternLen = s.length
    val next = ArrayBuffer.fill(patternLen)(0)
    var i = 0
    next(0) = -1 
    var j = -1
    while(i < patternLen-1){
      if(j == -1 || s(i) == s(j)){
        i=i+1
        j=j+1
        if(s(i) != s(j))  next(i) = j
        else  next(i) = next(j)
      }else
        j=next(j)
    }
    next
  }
}