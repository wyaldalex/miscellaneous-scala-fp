import scala.annotation.tailrec

object CodeWarsGreedIsGood extends App {

  /*
  Three 1's => 1000 points
  Three 6's =>  600 points
  Three 5's =>  500 points
  Three 4's =>  400 points
  Three 3's =>  300 points
  Three 2's =>  200 points
  One   1   =>  100 points
  One   5   =>   50 point
   */
  /*
  check for 1s and 5s first,
  then only triplets
   */
  val testCases = List(
    (List(2, 3, 4, 6, 2), 0),
    (List(1, 1, 1, 3, 3), 1000),
    (List(2, 2, 2, 3, 3), 200),
    (List(3, 3, 3, 3, 3), 300),
    (List(4, 4, 4, 3, 3), 400),
    (List(5, 5, 5, 3, 3), 500),
    (List(6, 6, 6, 3, 3), 600),
    (List(1, 1, 1, 1, 3), 1100),
    (List(1, 1, 1, 1, 5), 1150),
    (List(2, 4, 4, 5, 4), 450),
    (List(3, 4, 5, 3, 3), 350),
    (List(1, 5, 1, 3, 4), 250)
  )

  def score(dice: List[Int]): Int = {

    //special cases as singles have value
    val fives = dice.filter(_ == 5)
    val ones = dice.filter(_ == 1)
    //only count in triplets
    val twos = dice.filter(_ == 2)
    val threes = dice.filter(_ == 3)
    val fours = dice.filter(_ == 4)
    val sixes = dice.filter(_ == 6)
    val numberCollection = List(ones,twos,threes,fours,fives,sixes)

    def calculateScore(listToEval: List[Int]): Int = {
      listToEval match {
        case _ if listToEval.contains(1) =>
          if (listToEval.size == 3) 1000
          else if (listToEval.size > 3) 1000 + ((listToEval.size-3)*100)
          else 100*listToEval.size
        case _ if listToEval.contains(2) =>
          if (listToEval.size >= 3) 200 else 0
        case _ if listToEval.contains(3) =>
          if (listToEval.size >= 3) 300 else 0
        case _ if listToEval.contains(4) =>
          if (listToEval.size >= 3) 400 else 0
        case _ if listToEval.contains(5) =>
          if (listToEval.size == 3) 500
          else if (listToEval.size > 3) 500 + ((listToEval.size - 3) * 50)
          else 50 * listToEval.size
        case _ if listToEval.contains(6) =>
          if (listToEval.size >= 3) 600 else 0
        case _ => 0
      }
    }
    @tailrec
    def go(score:Int, remainingList: List[List[Int]]): Int ={
      remainingList match {
        case Nil => score
        case head :: tail => go(score + calculateScore(head ), tail)
      }
    }
    go(0,numberCollection)

  }
  testCases.foreach{
    x => println(score(x._1) == x._2)
  }

}
