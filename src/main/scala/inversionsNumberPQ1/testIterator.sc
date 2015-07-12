val strIter = "123".iterator
val multiStrIter =
  """
    |123
    |456
    |789
  """.stripMargin.iterator
val multiStrLines =
  """|123
    |456
    |789""".stripMargin.lines
/*val strIter1 = strIter
val strIter2 = strIter
val strIter3 = strIter
val strIter4 = strIter*/

//strIter.toArray
/*strIter
  .map(_.toInt)
  .toArray*/
/*strIter
  .toArray
  .map(_.toInt)*/
val intIter =
  for (elem<-strIter) yield elem.toInt

intIter
.toArray

val multiStrArray =
  multiStrLines
    .map(_.toInt)
.toArray

Seq(1,2,3) ++ Seq(4,5,6)
Seq(7, 5, 9, 4, 1, 3, 6, 2).sorted
Seq(3).take(1)
Seq(3).drop(1)
1 /2
3 / 2
5 / 2