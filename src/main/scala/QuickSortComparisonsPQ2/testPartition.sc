val someSeq: Seq[Int] =
  Seq(
  3, 1, 5, 2, 7, 6, 9, 8, 4
     )
/*0, 1,(2, 3,[4],5, 6),7, 8*/
5/2
someSeq
.sliding(5)
.toList
someSeq
.sliding(5, 5)
.toList
someSeq(2)
someSeq
.take(2)
someSeq
.drop(2)
someSeq
.drop(2 + 1)
someSeq
.partition(_ >= someSeq.head)
someSeq
.partition(_ == someSeq.head)
someSeq
.partition(_ <= someSeq.head)
someSeq
  .tail
.partition(_ <= someSeq.head)
someSeq
.partition(_ <= someSeq.last)
someSeq
  .init
.partition(_ <= someSeq.last)
someSeq
.partition(_ < someSeq.head)
someSeq
.partition(_ > someSeq.head)
someSeq
.partition(_ > -1)
someSeq
.partition(_ <= -1)
Seq().length
someSeq(4)
someSeq.slice(4,4+1)
someSeq.splitAt(4)
someSeq(0)
someSeq.slice(0,0+1)
someSeq.splitAt(0)
someSeq.length - 1
someSeq(someSeq.length - 1)
someSeq.slice(8,8+1)
someSeq.splitAt(8)