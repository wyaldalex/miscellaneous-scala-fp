import scala.annotation.tailrec

object ProofConcepts extends App {

  val someDic : List[List[Int]] = List(List(1))
  val last1st : List[List[Int]] = someDic :+ someDic.last
  val last2nd : List[List[Int]] = someDic :+ List(1,1)
  val last3rd : List[List[Int]] = last2nd :+ last2nd.last
  val last4th : List[List[Int]] = last3rd :+ last3rd.last
  val last5th : List[List[Int]] = last4th :+ last4th.last
  //println(last5th)
  //println(last4th.last)

  val row = 2
  def helper(lastRow: List[Int]): List[Int] = {
    if (lastRow.size - 1 > row) lastRow
    else if (lastRow.size - 1 <= row) List(1, 2) //continue building the list
    else List(1, 1)
  }
  println("passing: " + last2nd.last)
  println(helper( last2nd.last))

  println("General testing")
  val someList = List(1,2,1)
  println(someList.splitAt(someList.size/2))

  val someList2 = List(1, 4, 6,4,1)
  println(someList2.splitAt(someList2.size / 2))

  val someList3 = List(1, 5,10,10, 5, 1)
  println(someList3.splitAt(someList3.size / 2))

  //println(List(1,List(5,3,4).flatMap(_),1))
  println(1 +: 3 +: List (2) :+ 1 :+ 3 )
}


object recursiveNNumberPascal extends App {

  @tailrec
  def pascalRec(row: Int, col: Int, dic: List[List[Int]] = List(List(1))): Int = {
    println(s"Receving Dictionary $dic")
    @tailrec
    def helper(lastRow: List[Int]): List[Int] = {
      //no mas 1s
      //(1 +: (lastRow.tail.head + 1) +: lastRow.filter(x => x != 1) :+ (lastRow(lastRow.size - 1)) :+ 1)
      //1 +: List(3) :+ 3 :+ 1
      /* ((1 +: List(lastRow.tail.head + 1)) ++ lastRow.tail.tail
         .filter(x => x != 1)
         .map(x => x + dic(row)(col + 2)  ) ) :+ 1 :+ 1 */
      //1 +: (lastRow.tail.head +1) +:  List.empty :+ (lastRow.tail.head +1)  :+ 1
      //println(s"receiving $lastRow with row $row")
      //if (lastRow.size == 1) List(1, 1)
      //else if (lastRow.size == 2) List(1, 2, 1)
      /*
      else {
        helper(

          1 +: (lastRow.map(x => {
            if (x != 1) lastRow(col-1) + lastRow(col)
            else x
          }))
        )
      }
      helper( (1 +: ((1 + lastRow.tail.head) +: lastRow.tail)) ++ lastRow.splitAt(4)._2)


            else {
              val half = lastRow.splitAt(2)
              helper( (1 +: ((1 + lastRow.tail.head) +: lastRow.tail)) )
            }

             //println(s"Current Coordinate ($row, $col)")
              //helper((1 +: ((1 + lastRow.tail.head) +: lastRow.tail.tail.map(x => x + lastRow(col))) ) :+ 1)
              //asumiendo simetria
             helper(
               1 +:( ((1 + lastRow.tail.head) +: lastRow.tail.splitAt(lastRow.size)._1) ++
                 ((1 + lastRow.tail.head) +: lastRow.tail.splitAt(lastRow.size)._1).reverse)) :+ 1	)
       */
      if (lastRow.size - 1 >= row) lastRow
      else {
        println(s"Using lastRow $lastRow")
        helper(
          (0 +: lastRow).zip((lastRow) :+ 0).map(tup => {
            println(tup)
            tup._1 + tup._2
          })
        )
      }
    }
    if (dic.size - 1 >= row) dic(row)(col)
    else pascalRec(row, col, dic :+ helper(dic.last))
  }

  println(pascalRec(5,2 ))
}

/*

Inicio List(1)

Vector 1:   1  0
Vector 2:   0  1
Resultado:  1  1

Siguiente paso...

Vector 1:   1  1  0
Vector 2:   0  1  1
Resultado:  1  2  1

Siguiente paso...

Vector 1:   1   3  3  1  0
Vector 2:   0   1  3  3  1
Resultado:  1   4  6  4   1

Siguiente paso...

Vector 1:   1   3  3  1  0
Vector 2:   0   1  3  3  1
Resultado:  1   4  6  4   1

Siguiente paso...

Vector 1:   1  4  6  4  1  0
Vector 2:   0  1  4  6  4  1
Resultado:  1  5  10 10 5  1

 */

/*

fix  match form

@tailrec
  def pascalRec(row: Int, col: Int, dic: List[List[Int]] = List(List(1), List(1, 1))): Int = {
    @tailrec
    def helper(head: Int, tail: List[Int],
               acc: List[Int] = List(1)) : List[Int] = tail match {
      case h :: Nil => acc :+ 1
      case h :: t => helper(h, t, acc :+ (head + h))
    }
    if (dic.size - 1 >= row) dic(row)(col)
    else pascalRec(row, col, dic :+ helper(dic.last.head, dic.last.tail))
  }
  pascalPrint(4)
}
 */

object WeirdConstructs extends App {

  def someHigherAnon : (Int, String) => Int = (numba,name) => {
    name.length + numba
  }

  println(someHigherAnon(100,"fffff"))

}

object MapTesting extends App {

  val someMap : Map[String,Int] = Map("a" -> 100 , "b" -> 100)
  println(someMap + ("b" -> 201) )

  (-1000000 until 10).toSet

}