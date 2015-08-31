import scala.collection.immutable.TreeMap
val maxPQ1 = TreeMap(
5->6,1->2,7->8,3->4
                   )
maxPQ1.head
maxPQ1.min
maxPQ1.max
val minPQ2 =
  TreeMap(
           5->6,1->2,7->8,3->4
                   )
  //.ordering.reverse
(Ordering[Int].reverse)
minPQ2.head
minPQ2.min
minPQ2.max
val maxPQ1Iter = maxPQ1.iterator
maxPQ1Iter.take(5)
.map{case (k,v)=>k + "->>" + v}
//.toString()
.mkString(",")
val minPQ2Iter = minPQ2.iterator
minPQ2Iter.take(5)
.map{case (k,v)=>k + "->>" + v}
//.toString()
.mkString(",")
val minPQ3 =
  TreeMap
  .empty(Ordering[Int].reverse) +
    (
      5->6,1->2,7->8,3->4
      )
minPQ3.head
minPQ3.tail