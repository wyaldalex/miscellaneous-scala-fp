import scala.annotation.tailrec

object PascalTriangle extends App {
  /*
   The following pattern of numbers is called Pascal’s triangle.
       1
      1 1
     1 2 1
    1 3 3 1
   1 4 6 4 1
 1 5 10 10 5 1
1 6 15 20 15 6 1
   The numbers at the edge of the triangle are all 1, and each number inside
   the triangle is the sum of the two numbers above it. Write a function that computes
   the elements of Pascal’s triangle by means of a recursive process.
   Do this exercise by implementing the pascal function in Main.scala, which takes a
   column c and a row r, counting from 0 and returns the number at that spot in the triangle.
   For example, pascalRec(2,0)=1,pascalRec(2,1)=2 and pascalRec(3,1)=3
   */
  def pascalPrint(level: Int) = {
    for (row <- 0 to level) {
      for (col <- 0 to row)
        print(pascalRec(row, col) + " ")
      println()
    }
  }
  @tailrec
  def pascalRec(row: Int, col: Int, dic: List[List[Int]] = List(List(1))): Int = {
    @tailrec
    def helper(lastRow: List[Int]): List[Int] = {
      if (lastRow.size - 1 >= row) lastRow
      else {
        helper(
          (0 +: lastRow).zip((lastRow) :+ 0).map(tup => tup._1 + tup._2)
        )
      }
    }
    if (dic.size - 1 >= row) dic(row)(col)
    else pascalRec(row, col, dic :+ helper(dic.last))
  }
  pascalPrint(100)


  def pascalStackRec(row: Int, col: Int): Int = {
    if (col == 0 || col == row) 1
    else pascalRec(row - 1, col) + pascalRec(row - 1, col - 1)
  }

}