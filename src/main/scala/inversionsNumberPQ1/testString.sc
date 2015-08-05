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

val Array(tail, head) =
arcStr
.split(" ")
.map(_.toInt)
tail
head