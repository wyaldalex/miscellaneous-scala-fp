import scala.annotation.tailrec

object MoreTesting extends App {
  //Con arrays???
  //var myArray = Array[Array[Int]]
  def getAllLevelsTillN(level: Int) : Array[Array[Int]] = {

    var myMultiArray = Array.ofDim[Int](level,level)
    myMultiArray(0) = Array(1)
    @tailrec
    def go(currentLevel: Int, acc: Array[Array[Int]] ): Array[Array[Int]] = {
      //base case
      if (currentLevel == level) acc
      else {
        acc(currentLevel) = {
          var currentArray = Array.fill(currentLevel + 1){1}
          for(i <- 0 until   currentLevel) {
             currentArray(i) = 1
          }
          currentArray
        }
        go(currentLevel + 1, acc )
      }
    }
    if (level == 0) myMultiArray
    else go(1, myMultiArray)
  }
  //print
  val arrResult = getAllLevelsTillN(5)

  arrResult.foreach(x => {
    x.foreach(print(_))
    println()
    println("---------")
  })

}

object ScalaArrays extends App {
  val level = 100
  var myMultiArray = Array.ofDim[Int](level,level)
  println(myMultiArray)

}
