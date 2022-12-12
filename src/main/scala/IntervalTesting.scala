import scala.annotation.tailrec

object IntervalTesting extends App {

  def calculateCommon(evaluatedInterval: (Int, Int), intervalB: (Int, Int)): Int = {
    //evaluatedInterval is inside intervalB
    if ((evaluatedInterval._1 >= intervalB._1) && (evaluatedInterval._2 <= intervalB._2)) {
//      println(s"Was caught by inside interval $evaluatedInterval in $intervalB")
      evaluatedInterval._2 - evaluatedInterval._1
    }
    //intervalB is inside evaluatedInterval
    else if ((intervalB._1 >= evaluatedInterval._1) && (intervalB._2 <= evaluatedInterval._2)) {
//      println(s"Was caught by inside interval $intervalB in $evaluatedInterval")
      intervalB._2 - intervalB._1
    }
    //upper overlapping
    else if ((evaluatedInterval._1 >= intervalB._1) && (evaluatedInterval._1 <= intervalB._2) && (evaluatedInterval._2 > intervalB._2)) {
      println(s"Case of upper overlapping $evaluatedInterval against $intervalB")
      intervalB._2 - evaluatedInterval._1}
    //lower overlapping
    else if ((evaluatedInterval._1 < intervalB._1) && (evaluatedInterval._2 > intervalB._1) && (evaluatedInterval._2 < intervalB._2)) {
      println(s"Case of lower overlapping $evaluatedInterval against $intervalB")
      evaluatedInterval._2 - intervalB._1}
    else {
      //println(s"Was not caught by any case $evaluatedInterval against $intervalB")
      0
    }
  }

  def calculateOverlap(intervals: List[(Int, Int)]): Int = {

    @tailrec
    def overalapPerInterval(sum: Int, remainingList: List[(Int, Int)], evaluatedInterval: (Int, Int)): Int = {
      remainingList match {
        case Nil => sum
        case head :: tail => overalapPerInterval(sum + calculateCommon(evaluatedInterval, head), tail, evaluatedInterval)
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
  def overalapPerInterval(sum: Int, remainingList: List[(Int, Int)], evaluatedInterval: (Int, Int)): Int = {
    remainingList match {
      case Nil => sum
      case head :: tail => overalapPerInterval(sum + calculateCommon(evaluatedInterval, head), tail, evaluatedInterval)
    }
  }

  def overalapSumCalc(sum: Int, remainingList: List[(Int, Int)]): Int = {
    //base case? no more elements in the list
    remainingList match {
      case Nil => sum
      case head :: tail => overalapSumCalc(sum + overalapPerInterval(0, tail, head), tail)
    }
  }

  @tailrec
  def initialSumCalc(sum: Int, remainingList: List[(Int, Int)]): Int = {
    //base case? no more elements in the list
    remainingList match {
      case Nil => sum
      case head :: tail => initialSumCalc(sum + (head._2 - head._1), tail)
    }
  }

  val failedList = List((141, 244), (463, 465), (124, 429), (222, 304), (201, 451), (-386, 28), (497, 499), (-101, 233), (-95, -2), (145, 376), (-260, -241), (-499, -227), (-27, 499), (119, 228))
  println(s"Total Sum ${initialSumCalc(0,failedList)}")
  println(s"Total Rest ${overalapSumCalc(0,failedList)}")

}
