import scala.annotation.tailrec

object CodeWarsBouncingBall extends App {

  val testCases = List[(Double,Double,Double,Double)](
    (3, 0.66, 1.5, 3),
    (10, 0.6, 10, -1),
    (-5, 0.66, 1.5, -1),
    (5, -1, 1.5, -1)
  )

  def bouncingBall(h: Double, bounce: Double, window: Double): Int = {
    // your code
    //bounce is a factor
    @tailrec
    def go(nTimesPassesWindow: Int, currentBounceHeight: Double ): Int ={
      if (currentBounceHeight <= window) nTimesPassesWindow - 2
      else go(nTimesPassesWindow + 2, currentBounceHeight * bounce)
    }

    if(h < 0 || !(bounce > 0 && bounce < 1) || (window >= h)) -1
    else go(1,h)
  }

//  println(s"Test case result ${bouncingBall(3,0.66,1.5)}")
//  println(s"Test case result ${bouncingBall(2,0.5,1)}")
  testCases.foreach{
    x => println(s"Test case result ${bouncingBall(x._1,x._2,x._3)}")
  }

}
