val someSeq: Seq[Int] =
  Seq(
  3, 1, 5, 2, 7, 6, 9, 8, 4
     )

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