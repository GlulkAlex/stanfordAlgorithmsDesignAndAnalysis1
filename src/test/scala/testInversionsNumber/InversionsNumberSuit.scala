package testInversionsNumber

//import inversionsNumberPQ1.InversionsNumber.SortedSeqProps

import org.scalatest.FunSuite

//import inversionsNumberPQ1.InversionsNumber

/**
 * Created by Alex on 02.07.15.
 */
class InversionsNumberSuit extends FunSuite {

  import inversionsNumberPQ1.InversionsNumber

  //import InversionsNumberSuit

  //._
  trait Generator[+T] {
    self =>
    def generate: T

    def map[S](f: T => S): Generator[S] = new Generator[S] {
      def generate = f(self.generate)
    }

    def flatMap[S](f: T => Generator[S]): Generator[S] = new Generator[S] {
      def generate = f(self.generate).generate
    }
  }

  val integers = new Generator[Int] {
    val rand = new java.util.Random

    def generate = rand.nextInt()
  }

  def interval(lo: Int, hi: Int): Generator[Int] =
    for {x <- integers} yield lo + x % (hi - lo)

  ignore /*test*/ (
                    "1: An input size from file 'IntegerArray.txt' should " +
                      "have size 100 000 entries"
                  ) {
                      val inversionsNumber = new InversionsNumber
                      //assert(100000 == 100000)
                      assume(
                              //inversionsNumber
                              //InversionsNumber
                              //.countLines(
                              inversionsNumber
                              .getInput
                              .length
                                /*)*/ == 100000,
                              "must be '100 000'"
                            )
                    }
  ignore(
        "2: 'linesInput' Iterator should have size 100 000 entries"
      ) {
          val inversionsNumber = new InversionsNumber
          //assert(100000 == 100000)
          assume(
                  inversionsNumber
                  .linesInInputStream
                  .length == 100000,
                  "must be '100 000'"
                )
        }
  ignore(
        "3: 'linesInInputArray' should have size 100 000 entries"
      ) {
          val inversionsNumber = new InversionsNumber
          //assert(100000 == 100000)
          assume(
                  inversionsNumber
                  //.linesInInputArray
                  .integersInInputSeq
                  .length == 100000,
                  "must be '100 000'"
                )
        }
  ignore(
        "4: 'integersInInputArray' should have head > 0"
      ) {
          val inversionsNumber = new InversionsNumber
          //assert(100000 == 100000)
          assume(
                  inversionsNumber
                  .integersInInputArray
                  //.linesInInputArray
                  //.integersInInputSeq
                  .head == 54044 /*"54044"*//*!=""*//*>0*/ ,
                  "must be > '0'"
                )
        }
  ignore(
        "5: 'integersInInputArray' should have 'last' == '91901'"
      ) {
          val inversionsNumber = new InversionsNumber
          //assert(100000 == 100000)
          assume(
                  inversionsNumber
                  .integersInInputArray
                  //.linesInInputArray
                  //.integersInInputSeq
                  .lastIndexOf() == 91901 /*!=""*//*>0*/ ,
                  "must be == '91901'"
                )
        }
  ignore(
        "60: 'merge' should merge on 'empty sequence' & return 'empty sequence'"
      ) {
          val inversionsNumber = new InversionsNumber
          val unsorted =
          /*inversionsNumber
          .integersInInputSeq*/
          //Seq()
            Array.emptyIntArray
          /*(1, 2, 3, 4, 5, 6, 7, 9)*/
          /*
          swaps ?:
          1 2-1 (1, 4, 6, 9, 2, 3, 5, 7)
          2 4-2 (1, 2, 6, 9, 4, 3, 5, 7)
          3 6-4 (1, 2, 4, 9, 6, 3, 5, 7)
          4 4-3 (1, 2, 3, 9, 6, 4, 5, 7)
           */
          println(s"unsorted was:${unsorted.mkString( """,""")}")
          val swapCount: Int =
            inversionsNumber
            .mergeInArray(
                sourceArray =
                  unsorted
                         )

//          assume(
//                   inversionsNumber
//          .merge(
//              mergeLeftSide = unsorted.take(unsorted.length / 2),
//              mergeRightSide = unsorted.drop(unsorted.length -
//                                               unsorted.length / 2)
//                )
//                .sortedSeq == unsorted.sorted(Ordering[Int]),
          assume(
                  swapCount == 0 &&
                    unsorted.sameElements(unsorted.sorted),
                  "must be == List()"
                )
        }
  ignore(
        "61: 'merge' should merge on 'one element' & return that element"
      ) {
          val inversionsNumber = new InversionsNumber
          val unsorted =
          /*inversionsNumber
          .integersInInputSeq*/
            Seq(2)
          /*(1, 2, 3, 4, 5, 6, 7, 9)*/
          /*
          swaps ?:
          1 2-1 (1, 4, 6, 9, 2, 3, 5, 7)
          2 4-2 (1, 2, 6, 9, 4, 3, 5, 7)
          3 6-4 (1, 2, 4, 9, 6, 3, 5, 7)
          4 4-3 (1, 2, 3, 9, 6, 4, 5, 7)
           */

          //assert(100000 == 100000)
          assume(
                  inversionsNumber
                  .merge(
                      mergeLeftSide =
                        unsorted
                        //.take(/* unsorted.length / 2 */ 1),
                        .take(1),
                      mergeRightSide =
                        unsorted.drop(unsorted.length -
                                        unsorted.length / 2)
                        ).sortedSeq == unsorted.sorted,
                  "must be == List(2)"
                )
        }
  ignore(
        "62: 'merge' should merge 'two elements' & return sorted sequence"
      ) {
          val inversionsNumber = new InversionsNumber
          val unsorted =
          /*inversionsNumber
          .integersInInputSeq*/
            Seq(interval(0, 9).generate, interval(0, 9).generate)
          //Seq(9, 1)
          //Seq(1, 9)
          /*(1, 2, 3, 4, 5, 6, 7, 9)*/
          /*
          swaps ?:
          1 2-1 (1, 4, 6, 9, 2, 3, 5, 7)
          2 4-2 (1, 2, 6, 9, 4, 3, 5, 7)
          3 6-4 (1, 2, 4, 9, 6, 3, 5, 7)
          4 4-3 (1, 2, 3, 9, 6, 4, 5, 7)
           */
          println(s"unsorted:$unsorted")
          //assert(100000 == 100000)
          assume(
                  inversionsNumber
                  .merge(
                      mergeLeftSide = unsorted.take(unsorted.length / 2),
                      mergeRightSide = unsorted.drop(unsorted.length -
                                                       unsorted.length / 2)
                        ).sortedSeq == unsorted.sorted,
                  "must be == List(1, 2, 3, 4, 5, 6, 7, 9)"
                )
        }
  ignore(
        "63: 'merge' should merge 'three elements' & return sorted sequence"
      ) {
          val inversionsNumber = new InversionsNumber
          val unsorted =
          /*inversionsNumber
          .integersInInputSeq*/
            Seq(2, 4, 1)
          /*(1, 2, 3, 4, 5, 6, 7, 9)*/
          /*
          swaps ?:
          1 2-1 (1, 4, 6, 9, 2, 3, 5, 7)
          2 4-2 (1, 2, 6, 9, 4, 3, 5, 7)
          3 6-4 (1, 2, 4, 9, 6, 3, 5, 7)
          4 4-3 (1, 2, 3, 9, 6, 4, 5, 7)
           */

          //assert(100000 == 100000)
          assume(
                  inversionsNumber
                  .merge(
                      mergeLeftSide = unsorted.take(4),
                      mergeRightSide = unsorted.drop(4)
                        ).sortedSeq == unsorted.sorted,
                  "must be == List(1, 2, 3, 4, 5, 6, 7, 9)"
                )
        }
  ignore(
        "64: 'merge' should merge 'four elements' & return sorted sequence"
      ) {
          val inversionsNumber = new InversionsNumber
          val unsorted =
          /*inversionsNumber
          .integersInInputSeq*/
            Seq(2, 4, 1, 3)
          /*(1, 2, 3, 4, 5, 6, 7, 9)*/
          /*
          swaps ?:
          1 2-1 (1, 4, 6, 9, 2, 3, 5, 7)
          2 4-2 (1, 2, 6, 9, 4, 3, 5, 7)
          3 6-4 (1, 2, 4, 9, 6, 3, 5, 7)
          4 4-3 (1, 2, 3, 9, 6, 4, 5, 7)
           */

          //assert(100000 == 100000)
          assume(
                  inversionsNumber
                  .merge(
                      mergeLeftSide = unsorted.take(4),
                      mergeRightSide = unsorted.drop(4)
                        ).sortedSeq == unsorted.sorted,
                  "must be == List(1, 2, 3, 4, 5, 6, 7, 9)"
                )
        }
  ignore(
        "64: 'merge' should merge 'five elements' & return sorted sequence"
      ) {
          val inversionsNumber = new InversionsNumber
          val unsorted =
          /*inversionsNumber
          .integersInInputSeq*/
            Seq(2, 4, 6, 1, 3)
          /*(1, 2, 3, 4, 5, 6, 7, 9)*/
          /*
          swaps ?:
          1 2-1 (1, 4, 6, 9, 2, 3, 5, 7)
          2 4-2 (1, 2, 6, 9, 4, 3, 5, 7)
          3 6-4 (1, 2, 4, 9, 6, 3, 5, 7)
          4 4-3 (1, 2, 3, 9, 6, 4, 5, 7)
           */

          //assert(100000 == 100000)
          assume(
                  inversionsNumber
                  .merge(
                      mergeLeftSide = unsorted.take(4),
                      mergeRightSide = unsorted.drop(4)
                        ).sortedSeq == unsorted.sorted,
                  "must be == List(1, 2, 3, 4, 5, 6, 7, 9)"
                )
        }
  ignore(
        "70: 'mergeSort' should sort"
      ) {
          val inversionsNumber = new InversionsNumber
          val unsorted =
          /*inversionsNumber
          .integersInInputSeq*/
            Seq(2, 4, 6, 9, 1, 3, 5, 7)
          /*(1, 2, 3, 4, 5, 6, 7, 9)*/
          /*
          swaps ?:
          1 2-1 (1, 4, 6, 9, 2, 3, 5, 7)
          2 4-2 (1, 2, 6, 9, 4, 3, 5, 7)
          3 6-4 (1, 2, 4, 9, 6, 3, 5, 7)
          4 4-3 (1, 2, 3, 9, 6, 4, 5, 7)
           */

          //assert(100000 == 100000)
          assume(
                  inversionsNumber
                  .mergeSort(
                      unSortedSeq =
                        //Seq(2,1)
                        unsorted
                        .toStream
                            ).sortedSeq == unsorted.sorted,
                  "must be == List(1, 2, 3, 4, 5, 6, 7, 9)"
                )
        }
  ignore(
        "80: 'shiftElemRightUntilOrdered' should " +
          "place an element in sorted order"
      ) {
          val inversionsNumber = new InversionsNumber
          val unsorted: Array[Int] =
          /*inversionsNumber
          .integersInInputSeq*/
            Array(9, 1, 3, 5, 7)
          /*(1, 2, 3, 4, 5, 6, 7, 9)*/
          /*
          swaps ?:
          1 2-1 (1, 4, 6, 9, 2, 3, 5, 7)
          2 4-2 (1, 2, 6, 9, 4, 3, 5, 7)
          3 6-4 (1, 2, 4, 9, 6, 3, 5, 7)
          4 4-3 (1, 2, 3, 9, 6, 4, 5, 7)
           */
          println(s"unsorted was:${unsorted.mkString( """,""")}")
          //val sortedIndex: Int =
          val swapCount: Int =
            inversionsNumber
            .shiftElemRightUntilOrdered(
                sourceArray =
                  unsorted,
                elemIndex = 0,
                rangeStartIndex = 1,
                rangeEndIndex = 4,
                swapCount = 0
                                       )
          println(s"unsorted changes to:${unsorted.mkString( """,""")}")

          assume(
                  //sortedIndex == 4 &&
                  swapCount == 4 &&
                    /*unsorted == unsorted
                                .sorted,*/
                    unsorted.sameElements(unsorted.sorted),
                  "must be == List(1, 3, 5, 9)"
                )
        }
  //ignore
  test(
        "90: 'mergeArraySortedParts'" +
          "should properly merge & return ordered sequence"
      ) {
          val inversionsNumber = new InversionsNumber
          val unsorted: Array[Int] =
            //Array(7, 5)
            //Array(5, 7, 1)
            //Array(5, 7, 1, 2)
            Array(5, 7, 9, 11, 2, 3, 4)
          val (fP, sP) =
            inversionsNumber
            .partition(unsorted.length)

          println(s"unsorted was:${unsorted.mkString( """,""")}")
          val sorted: Array[Int] =
            inversionsNumber
            .mergeArraySortedParts(
                /*unchanging*/
                sourceArray = unsorted,
                /*unchanging*/
                sourceArrayLength = unsorted.length,
                /*changing ?*/
                currentPartSize =
                  //newPartSize,
                  fP,
                /*changing ?*/
                firstPartStart = 0,
                /*changing ?*/
                firstPartEnd = fP - 1,
                /*changing*/
                firstPartLeader = 0,
                /*changing ?*/
                secondPartStart = fP,
                /*changing ?*/
                secondPartEnd = unsorted.length - 1,
                /*changing*/
                //? redundant / useless ?
                //secondPartLeader: Int = 0,
                /*changing*/
                swapCount = 0
                                  )

          println(s"unsorted changes to:${unsorted.mkString( """,""")}")
          println(s"must be:${unsorted.sorted.mkString( """,""")}")
          /*too large for integer*/
          assume(
                  sorted
                  .sameElements(unsorted.sorted),
                  "must be equal"
                )
        }
}

