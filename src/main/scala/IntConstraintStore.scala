package strsolver


import ap.terfor.Formula

//store the input int constraints 
object IntConstraintStore{
  var formula : Formula = _
  def apply() = formula
  def setFormula(f : Formula) : Unit = {
    formula = f
  }
}
