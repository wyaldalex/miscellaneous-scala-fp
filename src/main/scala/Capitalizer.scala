object Capitalizer extends App {


  implicit class StringExtensions(s: String) {
    def toJadenCase = {
      s.split(" ").map(x => x(0).toUpper+x.takeRight(x.size - 1)).
        foldLeft("")((x,y) => x.concat(y.concat(" "))).take(s.size)
    }
  }

  println("How can mirrors be real if our eyes aren't real".toJadenCase)

}
