import scala.annotation.tailrec

object NarcissisticNumber extends App {

  def narcissistic(n: Int): Boolean = {
    def calculateSum(c: Char, nSize: Int): Int = {
      Math.pow(c.asDigit,nSize).toInt
    }
    @tailrec
    def go(accList: List[Char], sum: Int, nSize: Int ): Int = {
      accList match {
        case Nil => sum
        case head :: tail => go(tail, sum + calculateSum(head,nSize), nSize)
      }
    }
    go(n.toString.toList,0,n.toString.toList.size) == n
  }

  println(narcissistic(153))
  println(narcissistic(1652))
}
