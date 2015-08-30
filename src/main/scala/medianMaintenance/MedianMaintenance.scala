package medianMaintenance

import scala.collection.mutable

/**
 * Created by gluk-alex on 8/29/15.
 */
object MedianMaintenance {
  /*TODO
  The goal of this problem is to
  implement
  the "Median Maintenance" algorithm
  (covered in the Week 5 lecture on `heap` applications).
  The text file contains
  a list of the integers
  from '1' to '10000' in unsorted order;
  you should
  treat this as
  a `stream` of numbers,
  arriving one by one.
  Letting 'xi' denote
  the 'i-th' number of the file,
  the 'k-th' median 'm of k' is
  defined as
  the median of the numbers 'x1, ..., xk'.
  (So,
  if 'k' is `odd`, then
  'mk' is '((k + 1) / 2)'-th `smallest` number among 'x1, ..., xk';
  if 'k' is `even`, then
  'mk' is the '(k / 2)'-th `smallest` number among 'x1, ..., xk'.)

  In the box below
  you should
  type the `sum` of these '10000' medians,
  modulo '10000' (i.e., only the last '4' digits).
  That is,
  you should compute '(m1 + m2 + m3 + ... + m10000) mod 10000'.

  OPTIONAL EXERCISE:
  Compare the performance achieved by
  `heap-based` and
  `search-tree-based` implementations of the algorithm.
   */

  val inputEndSample: String = "2366\n558\n3536\n4855\n5940"

  /*
  also
  using 2 `heaps` partitioning:
  >1-st one with `extract-max` (for smaller / lesser numbers / digits)
  >2-nd one with `extract-min` (for greater / bigger numbers / digits)
   balanced by `half` of number of `elements` in each (as `invariant`),
   so
   with `adding` / every time when new `element`
   both heaps return `median`
   {[1,2,3][4,5,6]} (size even)
   (6 / 2) => 3-d element => index(2) => max of 1-st heap
   {[1,2,3][4,5,6,7]} (size odd)
   (7 + 1 / 2) => 4-th element => index(3 or 0) => min of 2-nd heap
   {[1,2,3,4][5,6,7]}
   (7 + 1 / 2) => 4-th element => index(3 or 0) => max of 1-st heap
   to keep `invariant` needed to
   redistribute unbalanced `elements` between `heaps`
   */
  class HeapsMedian {
    val heapMax = new
        mutable.PriorityQueue()(
                                 //implicit ord: Ordering[A]
                                 Ordering[Int]
                               )
    /*heapMax += (5,8,3,1,0,7,4,6)
    heapMax.head
    heapMax.dequeue()*/
    val heapMin = new
        mutable.PriorityQueue()(
                                 Ordering[Int].reverse
                               )

    def add(elem: Int): Unit = {
      /*val minMedian: Option[Int] =
        heapMax
        .headOption*/
      val maxMedian: Option[Int] =
        heapMin
        .headOption
      /*side effect*/
      if (
        maxMedian.isDefined &&
          maxMedian.get < elem
      ) {
        /*to the 2-nd `half`*/
        heapMin += elem//.toString.toInt
      } else {
        /*to the 1-st `half`*/
        heapMax += elem //.toString.toInt
      }

      val heapMaxLength: Int =
        heapMax
        .length
      val heapMinLength: Int =
        heapMin
        .length
      /*check & restore length invariant*/
      if (
        heapMaxLength > heapMinLength /* + 1*/ &&
          heapMaxLength > 1
      ) {
        val unbalancedElem: Int =
          heapMax
          .dequeue()
        /*side effect*/
        heapMin += unbalancedElem
      } else if (
              heapMaxLength + 1 < heapMinLength &&
                heapMinLength > 1
            ) {
        val unbalancedElem: Int =
          heapMin
          .dequeue()
        /*side effect*/
        heapMax += unbalancedElem
      } else {
        /*do nothing*/
      }
    }

    def addElem(elems: Int*): HeapsMedian = {
      /*side effect*/
      for (elem<-elems) {
        //val heapMaxLength: Int = heapMax.length
        /*side effect*/
        add(elem)
      }
      /*return value*/
      this
    }

    /*heaps must be balanced by size &
    as invariant
    all 'heapMax's elements must be
    less than or
    equal to any 'heapMin's element
    * */
    def addElems(elems: Seq[Int]): HeapsMedian = {
      /*side effect*/
      for (elem<-elems/*.toSeq*/) {
        add(elem)
      }
      /*return value*/
      this
    }

    def getMedian: Option[Int] = {
      if (heapMax.isEmpty && heapMin.isEmpty) {
        /*return value*/
        None
      } else {
        val heapMaxLength: Int = heapMax.length
        val heapMinLength: Int = heapMin.length
        val totalElements: Int =
          heapMaxLength + heapMinLength
        val minMedian: Option[Int] =
          heapMax
          .headOption
        val maxMedian: Option[Int] =
          heapMin
          .headOption

      if (totalElements % 2 != 0) {
        //odd
        if (heapMaxLength > heapMinLength) {
          /*return value*/
          //Some(
          minMedian

                //)
        } else {
          /*return value*/
          //Some(
          maxMedian
           //)
        }
        /*return value*/
      } else {
        //even
        assume(heapMaxLength == heapMinLength)
        /*return value*/
        minMedian
      }
    }
    }

    def getMedians: Option[(Int, Int)] = {
      val minMedian: Option[Int] =
        heapMax
        .headOption
      val maxMedian: Option[Int] =
        heapMin
        .headOption
      /*return value*/
      if (minMedian.isEmpty && maxMedian.isEmpty) {
        None
      } else if (minMedian.isDefined && maxMedian.isEmpty) {
        Some(minMedian.get, minMedian.get)
      } else if (minMedian.isEmpty && maxMedian.isDefined) {
        Some(maxMedian.get, maxMedian.get)
      } else /*if (minMedian.isDefined && maxMedian.isDefined)*/ {
        Some(minMedian.get, maxMedian.get)
      }
    }

  def showContent: String = {
    //this.
    heapMax.reverse.mkString("{[",",","]") +
    heapMin.mkString("[",",","]}")
  }

  }//class end

}
