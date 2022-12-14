import scala.annotation.tailrec

object HumanReadFormat extends App {

  val testCases = List( //seconds, expected
    (1, "1 second"),
    (62, "1 minute and 2 seconds"),
    (120, "2 minutes"),
    (3600, "1 hour"),
    (3662, "1 hour, 1 minute and 2 seconds")
  )


  def formatDuration(seconds: Int): String = {
    //start from top factor
    val yearFactor = (60 * 60) * 24 * 365f
    val dayFactor = (60 * 60) * 24f
    val hourFactor = 60 * 60f
    val minuteFactor = 60f

    val yearsKey = "years"
    val daysKey = "days"
    val hoursKey = "hours"
    val minutesKey = "minutes"
    val secondsKey = "seconds"

    def checkNewFactor(seconds: Int, currentFactor: Float): Float ={

      val factor = seconds/currentFactor.toInt
      factor match {
        case _ if factor < 1 => {
          if (currentFactor == yearFactor) dayFactor
          else if (currentFactor == dayFactor) hourFactor
          else if (currentFactor == hourFactor) minuteFactor
          else if (currentFactor == minuteFactor) 1
          else 1
        }
        case _ if factor >= 1 => currentFactor
        case _ => 0
      }
    }

    def subtractSecondsByFactor(factor: Float, remainingSeconds: Int): Int = {

      val validateFactor = remainingSeconds.toInt / factor.toInt
      validateFactor match {
        case 0 => remainingSeconds
        case _ if validateFactor >= 1 => (remainingSeconds - (validateFactor.toInt) * factor.toInt)
      }
    }

    def getCurrentUnitFactor(factor: Float, remainingSeconds: Int): (String,Int) = {
      val validateFactor = remainingSeconds / factor.toInt
      validateFactor match {
        case 0 => ("" -> 0)
        case _ if validateFactor >= 1 => {
          remainingSeconds / factor.toInt
          if( factor == yearFactor) (yearsKey -> (remainingSeconds / factor.toInt))
          else if ( factor == dayFactor) (daysKey -> (remainingSeconds / factor.toInt))
          else if ( factor == hourFactor) (hoursKey -> (remainingSeconds / factor.toInt))
          else if ( factor == minuteFactor) (minutesKey -> (remainingSeconds / factor.toInt))
          else ("" -> 0)
        }
      }
    }

    @tailrec
    def go(timesMap: Map[String,Int], currentFactor: Float, remainingSeconds: Int): Map[String,Int] = {
      remainingSeconds match {
        case _ if remainingSeconds == 0 => timesMap
        case _ if remainingSeconds < 60 => timesMap + (secondsKey -> remainingSeconds)
        case _ => {

          go(timesMap + getCurrentUnitFactor(currentFactor,remainingSeconds),
          checkNewFactor(remainingSeconds,currentFactor),
          subtractSecondsByFactor(currentFactor,remainingSeconds))
        }
      }
    }

    val orderedKeys: List[(String, String)] = List(yearsKey -> "year", daysKey -> "day", hoursKey -> "hour", minutesKey -> "minute", secondsKey -> "second")

    def decideIfMultiple(number: Int, tup: (String, String)): String = {
      number match {
        case _ if number > 1 => tup._1
        case _ => tup._2
      }
    }

    def stringAppender(str: String, timesMap: Map[String, Int], currentKey: (String, String)): String = {
      if (timesMap.contains(currentKey._1)) {
        timesMap.size match {
          case _ if timesMap.size > 1 =>
            str.concat(s"${timesMap(currentKey._1)} ${decideIfMultiple(timesMap(currentKey._1), currentKey)}, ")
          case _ => str.concat(s"${timesMap(currentKey._1)} ${decideIfMultiple(timesMap(currentKey._1), currentKey)}")
        }
      } else {
        str.concat("")
      }
    }

    @tailrec
    def buildListWithProperNaming(timesMap: Map[String, Int], counter: Int, builderString: String, orderedKeys: List[(String, String)], accList: List[String]): List[String] = {
      orderedKeys match {
        case Nil => accList
        case head :: tail => buildListWithProperNaming(timesMap, counter - 1, stringAppender(builderString, timesMap, head), tail, accList :+ stringAppender("", timesMap, head))
      }
    }

    if (seconds == 0) "now"
    else {
      val filteredMap = go(Map(), yearFactor, seconds).filter(tup => {
        tup._1 != ""
      })

      val listWithNamesAndVals = buildListWithProperNaming(filteredMap, filteredMap.size + 1, "", orderedKeys, List()).filter(x => x != "")

      val finalList = if (listWithNamesAndVals.size > 1) {
        val secondLast = listWithNamesAndVals(listWithNamesAndVals.size - 2).replace(",", " and")
        val lastOne = listWithNamesAndVals(listWithNamesAndVals.size - 1).replace(", ", "")
        val replacedTailList = listWithNamesAndVals.take(listWithNamesAndVals.size - 2) :+ secondLast :+ lastOne
        replacedTailList
      }
      else listWithNamesAndVals
      finalList.foldLeft("")(
        (x, y) => x.concat(y)
      )

    }
  }

  println(formatDuration(31536000*3))
  println(formatDuration(31535999))
  println(formatDuration(86399))
  testCases.foreach{
    x => println(formatDuration(x._1))
  }

  testCases.foreach {
    x => println(formatDuration(x._1) == x._2)
  }

}


object testInidividual extends App {
  def subtractSecondsByFactor(factor: Float, remainingSeconds: Int): Int = {
    val validateFactor = remainingSeconds / factor
    validateFactor match {
      case _ if validateFactor < 1 => remainingSeconds
      case _ if validateFactor >= 1 => (remainingSeconds - (validateFactor.toInt) * factor.toInt)
    }
  }

  def getCurrentUnitFactor(factor: Float, remainingSeconds: Int): Int = {
    val validateFactor = remainingSeconds / factor
    validateFactor match {
      case _ if validateFactor < 1 => remainingSeconds
      case _ if validateFactor >= 1 => remainingSeconds/factor.toInt
    }
  }

  def checkNewFactor(seconds: Int): Float = {
    val yearFactor = (60 * 60) * 24 * 365f
    val dayFactor = (60 * 60) * 24f
    val hourFactor = 60 * 60f
    val minuteFactor = 60f

    //Always start from top
    if (seconds/yearFactor > 1) yearFactor
    else if (seconds/dayFactor > 1) dayFactor
    else if (seconds/hourFactor > 1) hourFactor
    else if (seconds/minuteFactor > 1) minuteFactor
    else 1

  }

  println(checkNewFactor(31535999))
  println(Map() + ("" -> 0))

}
