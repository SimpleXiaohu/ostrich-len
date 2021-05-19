package strsolver.preprop
import ap.parser.ITerm
// import ap.parser

object LengthPreOp{
  def apply(length : ITerm): PreOp = {
    new LengthPreOp(length)
  }
}

class LengthPreOp(length : ITerm) extends PreOp{
	// TODO : maybe change the type of resultConstraint and  
	// type of returned res
 	override def toString = "length"
	def apply(argumentConstraints : Seq[Seq[Automaton]],
            resultConstraint : Automaton)
          : (Iterator[(Seq[Automaton], LinearConstraints)], Seq[Seq[Automaton]]) = {
          	val a = new LinearConstraints
            val builder = new BricsAutomatonBuilder
            val initState = builder.getNewState
            val Sigma = builder.LabelOps.sigmaLabel
            builder.setInitialState(initState)
            builder.setAccept(initState, true)
            builder.addTransition(initState, Sigma, initState)
            val res = builder.getAutomaton
            res.addNewRegister(1)
            res.addEtaMap((initState, Sigma, initState),List(1))
            a.addFormula( res.registers(0) === length )
          	(Iterator((Seq(res),a)), argumentConstraints)
    		}    
  def eval(arguments : Seq[Seq[Int]]) : Option[Seq[Int]] = 
  	// it may be inaccurate, but this is not matter in my algorithm, i don't use it 
  	Some(Seq(arguments(0).length))

}