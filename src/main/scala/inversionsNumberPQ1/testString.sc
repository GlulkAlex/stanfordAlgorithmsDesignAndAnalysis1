import scala.util.matching.Regex
"123"
"123".toInt
val tab = "" + '\u0009'
val mockAdjacencyList =
  List(1, 2, 7, 8).mkString("", tab, "\n") +
    List(2, 1, 8, 7, 3).mkString("", tab, "\n")

val mockIter =
mockAdjacencyList.lines
mockIter.next()
mockIter.next()
val arcStr: String = "875714 542453"
val arcStr2: String = "12  9"
val arcStr3: String = " 7  9"
//Returns
// a copy of the string,
// with leading and trailing `whitespace` omitted.
arcStr3.trim
val Array(tail, head) =
  arcStr
  .split(" ")
.map(_.toInt)
  tail
  head
val splitArray =
arcStr2
.split(" ")
//.map(s=>if(s.nonEmpty){s.toInt}else{Int.MinValue})
splitArray
splitArray
.map(_.replaceAll(" ",""))
splitArray
.map(_.replace(" ",""))
splitArray
.map(_.replace('\u0020',Char.MinValue))

val pattern1: Regex =
"""\d+""".r
val pattern2 =
"""(\d+)""".r
val pattern3 =
"""(\d+)(\d+)""".r
val pattern4 =
"""(\d+)\D(\d+)""".r
val arcTailHeadExtractor =
"""\s+(\d+)\s+(\d+).+""".r
def extractX(expr: String): String =
  expr match {
    case (arcTailHeadExtractor(arcTail,arcHead)) =>
    s"'arcTail' = $arcTail, 'arcHead' = $arcHead"
    case (pattern3(arcTail,arcHead)) =>
    s"'arcTail' = $arcTail, 'arcHead' = $arcHead"
    case pattern2(all @ _*) =>
      all mkString "/"
    case pattern3(all @ _*) =>
      all mkString "/"
    case _ => "no digits found"
  }
extractX(expr = """ 7  9""")
extractX(expr = arcStr2)
pattern1.findAllIn(arcStr2).mkString(",")
pattern1.findAllIn(arcStr2).toList
val List(arcTail, arcHead) =
pattern1
.findAllIn(
    arcStr
    //arcStr3
          )
  .map(_.toInt)
.toList
val digitsIter =
for (
  digits <- pattern1.findAllIn(arcStr3)
) yield digits.toInt
digitsIter.next
digitsIter.next
pattern1.split(arcStr2)
pattern1.findAllMatchIn(arcStr2)
pattern1.unapplySeq(arcStr2)
pattern2.unapplySeq(arcStr2)
pattern3.unapplySeq(arcStr2)
pattern2.findAllIn(arcStr2).mkString(",")
pattern3.findAllIn(arcStr2).mkString(",")
pattern4.findAllIn(arcStr2).mkString(",")
arcTailHeadExtractor.findAllIn(arcStr2).mkString(",")