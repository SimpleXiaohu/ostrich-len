package strsolver.preprop
import java.util.concurrent.atomic.AtomicInteger

import ap.terfor.ConstantTerm
// alloc constant term of register r. Id can not be greater than INTMAX
// e.g r = (R3, R4, R5)
object AllocRegisterTerm{
	val id : AtomicInteger = new AtomicInteger(0)
	/**
	 * alloc register constant term
	 */
	def apply() = {
		val number = id.get()
		val res = new ConstantTerm("R" + number)
		id.set(number+1)
		res
	 }
}

// alloc constant term of intermediate result t. 
// e.g t = (T3, T4, T5)
object AllocTTerm{
	val id : AtomicInteger = new AtomicInteger(0)
	/**
	 * alloc t constant term
	 */
	def apply() = {
		val number = id.get()
		val res = new ConstantTerm("T" + number)
		id.set(number+1)
		res
	 }
}
