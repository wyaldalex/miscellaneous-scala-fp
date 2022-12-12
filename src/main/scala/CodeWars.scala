import scala.annotation.tailrec

object CodeWars extends App {

  //The basic operation to validate if two intervals overlap
  // A=(1,10) B = (1,5)
  // A=(1,5) B = (1,10)
  //current failures (10,20),(16,19) should be
  //calculate Common should actually decrease the non unique parts
  def calculateCommon(evaluatedInterval: (Int, Int), intervalB: (Int, Int)): Int = {
    //evaluatedInterval is inside intervalB
    val toRest = if ((evaluatedInterval._1 >= intervalB._1) && (evaluatedInterval._2 <= intervalB._2)) {
      evaluatedInterval._2 - evaluatedInterval._1
    }
    //intervalB is inside evaluatedInterval
    else if ((intervalB._1 >= evaluatedInterval._1) && (intervalB._2 <= evaluatedInterval._2)) {
      intervalB._2 - intervalB._1
    }
    //upper overlapping
    else if ((evaluatedInterval._1 >= intervalB._1) && (evaluatedInterval._1 <= intervalB._2) && (evaluatedInterval._2 > intervalB._2)) intervalB._2 - evaluatedInterval._1
    //lower overlapping
    else if ((evaluatedInterval._1 < intervalB._1) && (evaluatedInterval._2 > intervalB._1) && (evaluatedInterval._2 < intervalB._2)) evaluatedInterval._2 - intervalB._1
    else {
      0
    }
    toRest
  }
  //Functional and immutable variables
  def sumOfIntervals(intervals: List[(Int, Int)]): Int = {

    def calculateOverlap(intervals: List[(Int, Int)]): Int = {

      @tailrec
      def overalapPerInterval(sum: Int, remainingList: List[(Int, Int)], evaluatedInterval: (Int, Int)): Int = {
        remainingList match {
          case Nil => sum
          case head :: tail => overalapPerInterval(sum + calculateCommon(evaluatedInterval, head), tail, head)
        }
      }

      @tailrec
      def overalapSumCalc(sum: Int, remainingList: List[(Int, Int)]): Int = {
        //base case? no more elements in the list
        remainingList match {
          case Nil => sum
          case head :: tail => overalapSumCalc(sum + overalapPerInterval(0, tail, head), tail)
        }
      }

      overalapSumCalc(0, intervals)
    }

    @tailrec
    def initialSumCalc(sum: Int,remainingList: List[(Int, Int)]): Int = {
      //base case? no more elements in the list
      remainingList match {
        case Nil => sum
        case head :: tail => initialSumCalc(sum + (head._2 - head._1),tail)
      }
    }

    @tailrec
    def overalapSumCalc(sum: Int, reaminingList: List[(Int, Int)]): Int = {
      //base case? no more elements in the list
      reaminingList match {
        case Nil => sum
        case head :: tail => overalapSumCalc(sum + (head._2 - head._1), tail)
      }
    }

    val initialSum = initialSumCalc(0,intervals)
    val restSum = calculateOverlap(intervals)

    initialSum - restSum
  }

  println("Initial sum test")
  val testList = List((1, 5),
    (10, 20),
    (1, 6),
    (16, 19),
    (5, 11))
  println(s"Summary result ${sumOfIntervals(testList)}")

  val testList2 = List((1, 2),
    (6, 10),
    (11, 15))
  println(s"Summary result 2 ${sumOfIntervals(testList2)}")

  val testList3 = List((1, 4),
    (7, 10),
    (3, 5))
  println(s"Summary result 2 ${sumOfIntervals(testList3)}")

  val testList4 = List((0, 20),
    (-100000000, 10),
    (30, 40))
  println(s"Summary result 2 ${sumOfIntervals(testList4)}")

  val failedList = List((141,244), (463,465), (124,429), (222,304), (201,451), (-386,28), (497,499), (-101,233), (-95,-2), (145,376), (-260,-241), (-499,-227), (-27,499), (119,228))
  val multipleRest = List((4,10),(7,13),(7,13),(7,13),(7,13))
  //UNique part algorithm
  // Unique parts of 4,10 = (4,5) (9,10) = 2
  // last (5,9) = (5,9) = 4
  val multipleRest2 = List((4,10),(5,9),(5,9),(5,9),(5,9))

  println(s"Multiple rest failed ${sumOfIntervals(multipleRest)}")
  println(s"Multiple rest failed 2 ${sumOfIntervals(multipleRest2)}")
  // 4,10 overlaped 7,13 -> 4,7

//  println(s"Summary negative ${calculateOverlapTest(testList)}")

//  println("Finding the failure")
//  @tailrec
//  def overalapPerIntervalTest(sum: Int, remainingList: List[(Int, Int)], evaluatedInterval: (Int, Int)): Int = {
//    //base case? no more elements in the list
//    remainingList match {
//      case Nil => sum
//      case head :: tail => overalapPerIntervalTest(sum + calculateCommon(evaluatedInterval, head), tail, evaluatedInterval)
//    }
//  }
//  println(s"Find error in overalp [1,5] ${overalapPerIntervalTest(0, List(
//    (10, 20),
//    (1, 6),
//    (16, 19),
//    (5, 11)),(1,5))}")

//  println(s"Find error in overalp [10, 20] ${
//    overalapPerIntervalTest(0, List(
//      (1, 6),
//      (16, 19),
//      (5, 11)), (1, 5))
//  }")
//  println(calculateCommon((10,20),(1,6)) == 0)
//  println(calculateCommon((10,20),(16,19)) == 3)
//  println(calculateCommon((10,20),(16,19)))



}
