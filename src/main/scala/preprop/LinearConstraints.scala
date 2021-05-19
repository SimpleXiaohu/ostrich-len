package strsolver.preprop
import ap.parser._

import scala.collection.mutable

class LinearConstraints{
	// the stored formula
	var store : List[IFormula] = List()
	def addFormula(f : IFormula) = {
		store = f :: store
	}
	// return the stored formula
	def apply() = store
	override def toString = store.toString
}

class StoreLC{
	// the stored formula
	val store = new mutable.HashSet[IFormula]()
	def addFormula(f : IFormula) = {
//		store = (f & store)
		store += f
	}

 	// return the stored formula
	def apply() = {
		var res : IFormula = IBoolLit(true)
		store.foreach{
			case f => res = f & res
		}
		res
	}
	def clean() = {
		store.clear()
	}

}