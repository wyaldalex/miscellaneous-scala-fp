import scala.annotation.tailrec
import scala.util.Random

object CodeWarsMovingZeroes extends App {

  def moveZeroes(lst: List[Int]): List[Int] = {

    def checkIfZero(acc: List[Int], position: Int): List[Int] = {
      lst(position) match {
        case 0 => acc
        case x: Int => acc :+ x
      }
    }

    @tailrec
    def generateZeroList(accList: List[Int], pendingZeroes: Int): List[Int] = {
      pendingZeroes match {
        case 0 => accList
        case x: Int => generateZeroList(accList :+ 0, pendingZeroes - 1)
      }
    }
    @tailrec
    def go(acc: List[Int], currentPos: Int): List[Int] = {
       currentPos match {
         case _ if currentPos > lst.size - 1 => acc
         case _ => go(checkIfZero(acc,currentPos), currentPos + 1)
      }
    }
    val nonZeroList = go(List(),0)
    nonZeroList match {
      case _ if nonZeroList.size < lst.size => nonZeroList ++ generateZeroList(List() ,lst.size - nonZeroList.size)
      case _ => nonZeroList
    }
  }
  println(moveZeroes(List(1,0,2,0,0,3)))
}
