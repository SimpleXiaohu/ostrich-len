package strsolver.preprop
import scala.collection.mutable

object Util{
  // ascii
  val CharMin : Char = 0
  val CharMax : Char = 127
	def subtractLettersSigma(set : mutable.SortedSet[Char]) : Iterable[(Char,Char)] = {
    val s = set.toBuffer
      val size = s.size
      var res = List[(Char, Char)]()
      var min = Char.MinValue
      val charMax = CharMax
      val charMin = CharMin
      // val charMax = 127.toChar
      // val charMin = 0.toChar

      if(size == 0){
        res = (charMin, charMax):: res
      }else{
        for(i <- 0 to size-2){
          if(min == s(i)){
            min = (s(i)+1).toChar
          }else{
            res = (min, (s(i)-1).toChar):: res
            min = (s(i)+1).toChar
          }
        }
        res = (min, (s(size-1)-1).toChar):: res
        if(s(size-1) != charMax)
        res = ((s(size-1)+1).toChar, charMax):: res
      }
      res
    }
}