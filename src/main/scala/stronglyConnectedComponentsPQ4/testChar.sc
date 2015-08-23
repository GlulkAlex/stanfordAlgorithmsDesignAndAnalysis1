val spaceChar = " "
val spaceAndDigitsChar = " 12 345"
//Char.is
//isLetter(spaceChar)
' '.isDigit
spaceChar.map(_.isDigit)
spaceAndDigitsChar.map(_.isDigit)
spaceAndDigitsChar
.collect({case d if d.isDigit => d})
spaceAndDigitsChar
.dropWhile(!_.isDigit)
val firstDigitStr =
  spaceAndDigitsChar
.dropWhile(!_.isDigit)
.takeWhile(_.isDigit)
spaceAndDigitsChar
.drop(spaceAndDigitsChar.length -
        firstDigitStr.length - 1)
.dropWhile(!_.isDigit)
.takeWhile(_.isDigit)
@scala.annotation.tailrec
def extractPairOfDigits(
                         sourceStr: String,
                       digit1: Option[String] = None,
                       digit2: Option[String] = None,
                       flag: Boolean = false
                         ):
(Option[Int],Option[Int]) = {
//Option[(Int,Int)] = {
  if (sourceStr.isEmpty) {
    (
      digit1 match {
        case Some(d: String) => Some(d.toInt)
        case _ => None
      },
      /*.map((s):String=>Some(s.toInt))*/
      //.collect({case Some(d) => Some(d.toInt)}),
      /*.map({
             case d: String => Some(d.toInt)
             case _ => None
           }),*/
    if (digit2.isEmpty) {
      None
    } else {
      Some(digit2.get.toInt)
    }
      )
  } else {
    val (newDigit1,newDigit2,newFlag):
    (Option[String],Option[String],Boolean)=
    if (sourceStr.head.isDigit) {
      if (flag) {
        /*add to second*/
        (digit1,
          Some(digit2.getOrElse("") + "" +
                 sourceStr.head),
          flag)
      } else {
        /*add to first*/
        (Some(digit1.getOrElse("") + "" +
                sourceStr.head),
          digit2,
          flag)
      }
    } else {
      if (
        digit1.isDefined &&
        digit2.isEmpty
      ) {
        (digit1, digit2, true)
      } else {
        (digit1, digit2, flag)
      }
    }
    /*recursion*/
    extractPairOfDigits(
                         sourceStr.tail,
                         newDigit1,
                         newDigit2,
                         newFlag
                       )
  }
}
extractPairOfDigits(sourceStr=spaceChar)
extractPairOfDigits(sourceStr=spaceAndDigitsChar)