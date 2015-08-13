import scala.collection.immutable.Queue

var nodesQueue: scala.collection.immutable.Queue[Int] =
  Queue.empty
//List(startNodeKey)
nodesQueue = nodesQueue.enqueue(1)
val nodesQueue2: scala.collection.immutable.Queue[Int] =
  Queue(2)
val nodesQueue3: scala.collection.immutable.Queue[Int] =
  Queue(1,2,3)
val (firstTopElem, restOfQueue) =
  nodesQueue3.dequeue