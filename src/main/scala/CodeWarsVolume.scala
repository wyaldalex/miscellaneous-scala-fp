import scala.annotation.tailrec

object CodeWarsVolume extends App {

  val testCases = List(
    (4183059834009L, 2022),
    (24723578342962L, -1),
    (135440716410000L, 4824),
    (40539911473216L, 3568),
    (26825883955641L, 3218)
  )
  def findNb(m: Long): Int = {

    def cubicLong(n: Long): Long = {
      n * n * n
    }

    @tailrec
    def go(currentVol: Long, currentIteration: Int): Int ={
      currentVol match {
        case _ if (currentVol == m) => currentIteration - 1
        case _ if (currentVol > m) => - 1
        case _ => go(currentVol + cubicLong (currentIteration.toLong), currentIteration + 1)
      }
    }
    go(0,0)
  }
  println(findNb(1071225l))
  println(findNb(91716553919377l))
  println(findNb(40539911473216L))
  testCases.foreach{
    x => println(findNb(x._1))
  }

}

