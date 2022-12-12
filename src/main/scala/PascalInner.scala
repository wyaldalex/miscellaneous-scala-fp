import scala.annotation.tailrec

object PascalInner extends App {

  val innerList = List(1,3,3,1)
  val innerList2 = List(1,5,10,10,5,1)
  val innerList3 = List(1, 6, 15, 20, 15,6, 1)

  def constructingList(accList: List[Int] , counter: Int, originalList: List[Int]): List[Int] = {
    accList match {
      case _ if counter < originalList.size - 1 => accList :+ (originalList(counter) + originalList(counter+1))
      case _ => accList
    }
  }

  @tailrec
  def createNextRow(accList: List[Int], targetSize: Int, counter: Int, prevRow: List[Int] ): List[Int] = {
    counter match {
      case _ if counter == targetSize => 1 +: accList :+ 1
      case _ => createNextRow( constructingList(accList,counter,prevRow), targetSize, counter + 1, prevRow)
    }
  }

  println(createNextRow(List(),4,0,innerList))
  println(createNextRow(List(),5,0,innerList2))
  println(createNextRow(List(),6,0,innerList3))

  @tailrec
  def MegaWrapper(level: Int, accList: List[List[Int]], counterLevel: Int): List[Int] = {
    level match {
      case 0 => List(1)
      case _ if level == counterLevel => accList(level - 1)
      case _ => MegaWrapper(level, accList :+ createNextRow(List(),counterLevel,0,accList(counterLevel)), counterLevel + 1)
    }

  }
  println(MegaWrapper(8,List(List(1)),0))

}
