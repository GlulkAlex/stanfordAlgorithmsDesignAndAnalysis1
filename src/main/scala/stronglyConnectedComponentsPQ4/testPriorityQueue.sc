import scala.collection.mutable

//5,8,3,1,0,7,4,6
val heapMax = new
    mutable.PriorityQueue()(
//implicit ord: Ordering[A]
Ordering[Int]
)
heapMax += (5,8,3,1,0,7,4,6)
heapMax.head
heapMax.dequeue()
heapMax
val heapMin = new
    mutable.PriorityQueue()(
//implicit ord: Ordering[A]
Ordering[Int].reverse
)
heapMin += (5,8,3,1,0,7,4,6)
heapMin.head
heapMin.dequeue()
heapMin.head
heapMin