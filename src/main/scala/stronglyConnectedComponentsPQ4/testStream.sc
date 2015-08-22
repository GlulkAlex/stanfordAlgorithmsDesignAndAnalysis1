val stream1 = Stream(1, 2, 3)
val stream2 = Stream(3, 4, 5)
val stream3 = Stream(6, 7, 8)
stream1.diff(stream2).mkString(",")
stream1.intersect(stream2).mkString(",")
stream2.intersect(stream1).mkString(",")
stream2.intersect(stream3).mkString(",")
val streamOfStreams1 = Stream(stream1, stream2, stream3)
streamOfStreams1
.map(n => n.mkString("{", ",", "}"))
.mkString(",")
streamOfStreams1.
map(stream => stream.map(_ => 1))
val allStreamsValues =
  streamOfStreams1
//.flatMap(_ => List(1))
.flatMap(_.map(_ => 1))
allStreamsValues
.mkString(",")
allStreamsValues.sum
stream1.size + stream2.size + stream3.size