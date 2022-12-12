import scala.annotation.tailrec

object CodeWars2 extends App {

  def sumOfIntervals(intervals: List[(Int, Int)]): Int = {

    @tailrec
    def go(accumulatorList: Set[Int], remainingList: List[(Int, Int)]): Int = {
      remainingList match {
        case Nil => accumulatorList.size
        case head :: tail => go(accumulatorList ++ (head._1 until head._2).toSet,tail)
      }
    }
    go(Set(),intervals)
  }

    println("Initial sum test 1")
    val testList = List((1, 5),
      (10, 20),
      (1, 6),
      (16, 19),
      (5, 11))
    println(s"Summary result 2 ${sumOfIntervals(testList)}")

    val testList2 = List((1, 2),
      (6, 10),
      (11, 15))
    println(s"Summary result 3 ${sumOfIntervals(testList2)}")

    val testList3 = List((1, 4),
      (7, 10),
      (3, 5))
    println(s"Summary result 4 ${sumOfIntervals(testList3)}")

    val testList4 = List((0, 20),
      (-100000000, 10),
      (30, 40))
    println(s"Summary result  5 ${sumOfIntervals(testList4)}")

  //Set test
  val initialSet = (1 until 11).toSet
  //println(initialSet + (2 to 7))
  println(s"Initial set size ${initialSet.size}")
  val set2 = initialSet ++ Set(3,4,5) // +0
  val set3 = set2 ++ Set(-1,0,1,2,3) // + 2
  val set4 = set3 ++ Set(100,101,102,103) // + 4
  val set5 = set4 ++ Set(-100,-99,-98,-97) // + 4

  println(s"Final set size ${set5.size}")

  //Converting a range to a set
}


object SetTesting extends App {

  val someSet = (-1000000000 to 1000000000).toSet
  println(someSet ++ Set(1,2,3))
}

