import scala.annotation.tailrec

object CompareSameSquaresArrays extends App {

  def comp(seq1: Seq[Int], seq2: Seq[Int]): Boolean = {

    def checkParameters(num: Int,squaredSeq:Seq[Int]): Boolean = {
      val squaredNum = Math.pow(num,2)
      if (squaredSeq.contains(squaredNum)) {
        if(seq1.filter(x => x == num).size == squaredSeq.filter(x => x == squaredNum).size) true
        else false
      } else false
    }
    @tailrec
    def go(remainingList:Seq[Int], squaredSeq: Seq[Int], accumulatorList: List[Boolean] ): List[Boolean] = {
      remainingList match {
        case Nil => accumulatorList
        case head :: tail => go(tail,squaredSeq,accumulatorList :+ checkParameters(head,squaredSeq))
      }
    }



    if (seq1 == Nil && seq2 == Nil) true
    else if (seq1 == Nil || seq2 == Nil) false
    else go(seq1.toList,seq2.toList, List()).filter(x => x == false ).size == 0
  }

  println(comp(List(121, 144, 19, 161, 19, 144, 19, 11),List(121, 14641, 20736, 361, 25921, 361, 20736, 361)))
  println(comp(List(121, 144, 19, 161, 19, 144, 19, 11),List(132, 14641, 20736, 361, 25921, 361, 20736, 361)))

  println(comp(Vector(78, 50, 67, 66, 41, 67), Vector(2500, 1681, 6084, 4356, 4489, 4490)))

  println(comp(List(2, 2, 3), List(4, 9, 9))) //repetitions
  println(comp(List(121, 144, 19, 161, 19, 144, 19, 11, 1008), List(121, 14641, 20736, 36100, 25921, 361, 20736, 361)))

  println(comp(List(), List()))
  println(List() == Nil)
}
