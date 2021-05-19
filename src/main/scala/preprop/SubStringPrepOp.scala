package strsolver.preprop
import ap.basetypes.IdealInt
import ap.parser.{IFormula, ITerm, Internal2InputAbsy}
import ap.terfor.{OneTerm, Term, linearcombination}
import ap.terfor.linearcombination.LinearCombination

import scala.collection.mutable.SortedSet

object SubStringPreOp{
  def apply(i : Term, j : Term, xlen : Term, reslen : Term) : PreOp = {
    new SubStringPreOp(i,j,xlen, reslen)
  }
}


class SubStringPreOp(i : Term, j : Term, xlen : Term, reslen : Term) extends PreOp{
  def getSpecialPre(j: ITerm, reslen: ITerm, xlen:ITerm, resAut: BricsAutomaton) : Iterator[(Seq[Automaton], LinearConstraints)] = {
    val a = new LinearConstraints
    val b = resAut.getBuilder
    val tmpList = List.fill(resAut.registers.size)(0)
    val states = List.fill(resAut.states.size)(b.getNewState)
    val sigmaS = b.getNewState
    val sigma = b.LabelOps.sigmaLabel
    val statesmap = resAut.states.zip(states).toMap
    b.setInitialState(statesmap(resAut.initialState))
    for(s <- resAut.states){
      for((to,label) <- resAut.outgoingTransitions(s)){
        val vector = resAut.etaMap((s, label, to))
        b.addTransition(statesmap(s), label, statesmap(to), List(1)++vector)
      }
      if(resAut.isAccept(s)){
        b.setAccept(statesmap(s), true)
        val lables = SortedSet[Char]()
        for((_,lable) <- resAut.outgoingTransitions(s)) {
          lables += lable._1
          lables += lable._2
        }
        val otherLable = Util.subtractLettersSigma(lables)
        otherLable.foreach(
          b.addTransition(statesmap(s), _, sigmaS, List(0)++tmpList)
        )

      }
    }
    b.setAccept(sigmaS, true)
    b.addTransition(sigmaS, sigma, sigmaS, List(0)++tmpList)
    val res = b.getAutomaton
    res.addNewRegister(1)
    res.addRegisters(resAut.registers)
    res.addEtaMaps(b.etaMap)
    a.addFormula( (
      (res.registers(0) === j) & // for cvc4 , j is offset, position is i+j
        (res.registers(0) <= xlen)
      ) |
      (
        (res.registers(0) === xlen) &
          (j > reslen)   // for cvc4, if i+j > s.len, the substr(s,i, s.len)
        ) |
      (
        // handle ""
        (j===0 | xlen === 0) &
        (reslen === 0)
        )
    )
    a.addFormula(j>=0)
    Iterator((Seq(res), a))
  }
	def apply(argumentConstraints : Seq[Seq[Automaton]],
            resultConstraint : Automaton)
          : (Iterator[(Seq[Automaton], LinearConstraints)], Seq[Seq[Automaton]]) = {
    val input_i = Internal2InputAbsy(i)
    val input_j = Internal2InputAbsy(j)
    val input_xlen = Internal2InputAbsy(xlen)
    val input_reslen = Internal2InputAbsy(reslen)
    val a = new LinearConstraints
    var resAut = AtomicStateAutomatonAdapter.intern(resultConstraint).asInstanceOf[BricsAutomaton]
    // "" = substring(x, i, j)
    if(resAut.underlying.isEmptyString() || reslen == LinearCombination.ZERO){
//       i<0 | j <= 0 | i >= len(x)
      a.addFormula((input_i < 0) | (input_j===0) | input_i >= input_xlen)
      return (Iterator((Seq(BricsAutomaton.makeAnyString()), a)), List())
    }
    val resContainElp = resAut.underlying.getShortestExample(true) == ""
    //////////////////////////////////////
    // j == 1, for example, substr derived from str.at
    val b = resAut.getBuilder
    val initState_ = b.getNewState
    b.setInitialState(initState_)
    for((to, label) <- resAut.outgoingTransitions(resAut.initialState)){
      val state = b.getNewState
      if(resAut.isAccept(to)){
        b.setAccept(state, true)
      }
      val vector = resAut.etaMap((resAut.initialState, label, to))
      b.addTransition(initState_, label, state, vector)
    }
    val res1 = b.getAutomaton
    res1.addEtaMaps(b.etaMap)
    res1.addRegisters(resAut.registers)
    //////////////////////////////////////
    if(i == LinearCombination.ZERO){
      // i == 0
      if(j == OneTerm){
        val res = BricsAutomaton.concat(res1,BricsAutomaton.makeAnyString())
        if(resContainElp){
          return (Iterator((Seq(res),a),(Seq(BricsAutomaton.fromString("")),a)),List())
        }else {
          return (Iterator((Seq(res), a)), List())
        }
      }else {
       val res = getSpecialPre(input_j, input_reslen, input_xlen, resAut)
       return (res, List())
      }
    }
    if(i.toString == xlen.toString+" + -1" && j == OneTerm){
      // i = xlen - 1, i.e the last position of string x
      val res = BricsAutomaton.concat(BricsAutomaton.makeAnyString(), res1)
      if(resContainElp){
        return (Iterator((Seq(res),a),(Seq(BricsAutomaton.fromString("")),a)),List())
      }else {
        return (Iterator((Seq(res), a)), List())
      }
    }
    // TODO : optimize
    if( j == OneTerm ){
      resAut = res1
      if(resContainElp)
        resAut.initialState.setAccept(true)
    }

    val builder = resAut.getBuilder
    val infiniteCycleS = builder.getNewState
    builder.setAccept(infiniteCycleS, true)
    val resAutRLen = resAut.registers.size    // resAut registers size
    val tmpList = List.fill(resAutRLen)(0)
    val sigma = builder.LabelOps.sigmaLabel

    val resAutSLen = resAut.states.size       // resAut states size
    val states1 = List.fill(resAutSLen)(builder.getNewState)
    // from resAut states map to new states
    val sMap = (resAut.states zip states1).toMap
    val initState = builder.getNewState
    builder.setInitialState(initState)
    val resInit = resAut.initialState
    if(resAut.isAccept(resInit))
      builder.setAccept(initState, true)

//    var needIFCS : Boolean = false
    // add transition from resAut initState
    for((ts, (charMin, charMax)) <- resAut.outgoingTransitions(resInit)){
      val vector = resAut.etaMap((resInit, (charMin,charMax), ts))
      builder.addTransition(initState, (charMin,charMax), sMap(ts), List(0,1)++vector)
    }

    builder.addTransition(initState, sigma, initState, List(1,1)++tmpList)

    var resInitArrivable = false
    for((_, _, to) <- resAut.transitions){
      if(to == resInit)
      resInitArrivable = true
    }
    for(fs <- resAut.states){
      val fsIsAccept = resAut.isAccept(fs)
//      builder.setAccept(sMap(fs), fsIsAccept)
      if(resInitArrivable || fs != resInit) {
        // if resInit not arrivable,
        // resInit's transition has already been added above
        for ((ts, label) <- resAut.outgoingTransitions(fs)) {
          val vector = resAut.etaMap((fs, label, ts))
          builder.addTransition(sMap(fs), label, sMap(ts), List(0, 1) ++ vector)
        }
        if (fsIsAccept) {
          builder.setAccept(sMap(fs), true)
          val lables = SortedSet[Char]()
          for((_,lable) <- resAut.outgoingTransitions(fs)) {
            lables += lable._1
            lables += lable._2
          }
          val otherLable = Util.subtractLettersSigma(lables)
          otherLable.foreach(
            builder.addTransition(sMap(fs), _, infiniteCycleS, List(0,0) ++ tmpList)
          )
        }
      }
    }
    builder.addTransition(infiniteCycleS,sigma,infiniteCycleS, List(0,0)++tmpList)
    val res = builder.getAutomaton
    res.addEtaMaps(builder.etaMap)
    res.addNewRegister(2)   // i,j
    res.addRegisters(resAut.registers)  // New registers is (i, j)++resAut.registers
    if(resContainElp){
      // res contains ""
      a.addFormula( ( ( (
                          (res.registers(1) === (input_i+input_j)) & // for cvc4 , j is offset, position is i+j
                          (res.registers(1) <= input_xlen)
                        ) |
                        (
                          (res.registers(1) === input_xlen) &
                          (input_i+input_j > input_reslen )   // for cvc4, if i+j > s.len, the substr(s,i, s.len)
                        )
                      ) &
                      (res.registers(0) === input_i)
                    ) |
                    // logic for res is ""
                    (
                      ((input_i < 0) | (input_j===0)| input_xlen <= input_i) &
                      (input_reslen === 0)
                    )
                  )
    }else{
      a.addFormula( ( (
                        (res.registers(1) === (input_i+input_j)) & // for cvc4 , j is offset, position is i+j
                        (res.registers(1) <= input_xlen)
                      ) |
                      (
                        (res.registers(1) === input_xlen) &
                        (input_i + input_j > input_reslen)   // for cvc4, if i+j > s.len, the substr(s,i, s.len)
                      )
                    ) & 
                    (res.registers(0) === input_i)
                  )
    }
    a.addFormula(input_j>=0)
    (Iterator((Seq(res), a)), List())
  }

  def eval(arguments : Seq[Seq[Int]]) : Option[Seq[Int]] = {

    val strLen = arguments(0).size
      val i_option = IdealInt.unapply(i.asInstanceOf[LinearCombination].constant)
      val j_option = IdealInt.unapply(j.asInstanceOf[LinearCombination].constant)
      if (i_option == None || j_option == None)
        return None
      val i_value = i_option.get
      val j_value = j_option.get
      if(i_value < 0 || j_value > strLen-1)
        return None

      return Some(arguments(0).slice(i_value, j_value))

    }
 	override def toString = "substring{"+i.toString+","+j.toString+"}"

}