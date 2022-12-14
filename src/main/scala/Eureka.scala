import scala.annotation.tailrec

object Eureka extends App {

  def sumDigPow(a: Long, b: Long): List[Long] = {

    def checkParameter(i: Long): Boolean = {
      i == i.toString.zipWithIndex.map(x => (x._1).asDigit -> (x._2 + 1))
        .foldLeft(0)((x,y) => x + Math.pow(y._1,y._2).toInt)
    }

    @tailrec
    def go(currentN: Long, accList: List[Long]): List[Long] ={
      currentN match {
        case _ if currentN > b =>  accList
        case _ => go(currentN + 1,if(checkParameter(currentN)) accList :+ currentN else accList)
      }
    }
    go(a,List())
  }

  //println(89l.toString.zipWithIndex.toList)

  def checkParameter(i: Long): Boolean = {
    i == i.toString.zipWithIndex.map(x => (x._1).asDigit -> (x._2 + 1))
      .foldLeft(0)((x, y) => x + Math.pow(y._1, y._2).toInt)
  }
//  println(checkParameter(1))
//  println(checkParameter(2))
//  println(checkParameter(3))
//  println(checkParameter(4))
//  println(checkParameter(5))
//  println(checkParameter(6))
//  println(checkParameter(7))
//  println(checkParameter(8))
//  println(checkParameter(9))
//  println(checkParameter(10))
//  println(checkParameter(40))
//  println(checkParameter(135))
//  println(checkParameter(89))
  println(sumDigPow(1,100))
  println(sumDigPow(1,200))
  println(sumDigPow(90,100))
  println(sumDigPow(90,135))



}
