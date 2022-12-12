import scala.::
import scala.annotation.tailrec

object BasicsRecap extends App {


  def linearRecursion[A](l: List[A]): List[A] = l match {
    case h :: tail => linearRecursion(tail) ::: List(h)
    case Nil => Nil
  }

  def tailRecursion[A](l: List[A]): List[A] = {
    //also called helper
    //accumulator in this case is modifiedList
    @tailrec
    def go(originalList: List[A],modifiedList: List[A]): List[A] = {
      originalList match {
        case Nil => modifiedList
        case h :: tail =>  go(tail, h +: modifiedList)
      }
    }
    go(l, List())
  }


  val someList = List(1,3,4,5,6,7,8,0)
  //val transformedList = linearRecursion(someList)
  val transformedList = tailRecursion(someList)

  someList match {
    case h :: tail => println(s"The head is ${h} and the tail is $tail") //this is used to check if there is still something

  }

  println(s"Original list $someList")
  println(s"Transformed list $transformedList")

  println(someList ::: transformedList) //so the ::: dots means kindaFlatMapping the list, something like that.
  println(1000 :: someList )


}
