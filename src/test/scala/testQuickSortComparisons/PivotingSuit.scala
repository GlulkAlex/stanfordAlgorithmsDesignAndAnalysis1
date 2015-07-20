package testQuickSortComparisons

import org.scalatest.FunSuite
import QuickSortComparisonsPQ2.Pivoting1FirstElement._

//import inversionsNumberPQ1.InversionsNumber

/**
 * Created by gluk-alex on 7/17/15.
 */
class PivotingSuit
  extends FunSuite {

  ignore(
          "1: 'getInput'" +
            "should read from file"
        ) {
            //val inversionsNumber = new InversionsNumber
            val unsorted: Array[Int] =
              getInput()
              .map(_.toInt)
              .toArray
            //Array(7, 5)
            //Array(5, 7, 1)
            //Array(5, 7, 1, 2)
            //Array(5, 7, 9, 11, 2, 3, 4)

            //println(s"unsorted was:${unsorted.take(10).mkString( """,""")}")
            /*too large for integer*/
            assume(
                    //true == true,
                    unsorted.length == 10000,
                    "must be equal"
                  )
          }
  ignore(
          "11: 'ChoosePivot'" +
            "should return head for nonEmpty Array"
        ) {
            //val inversionsNumber = new InversionsNumber
            val unsorted: Array[Int] =
            /*getInput()
            .map(_.toInt)
            .toArray*/
            //Array(7, 5)
            //Array(5, 7, 1)
            //Array(5, 7, 1, 2)
              Array(5, 7, 9, 11, 2, 3, 4)

            //println(s"unsorted was:${unsorted.take(10).mkString( """,""")}")
            /*too large for integer*/
            assume(
                    //true == true,
                    ChoosePivot(unsorted,
                                unsorted.length) == unsorted.head,
                    "must be equal"
                  )
          }
  ignore(
          "21: 'QuickSort'" +
            "should return sorted Array"
        ) {
            //val inversionsNumber = new InversionsNumber
            val unsorted: Array[Int] =
              getInput()
              .map(_.toInt)
              .toArray
            //Array(7, 5)
            //Array(5, 7, 1)
            //Array(5, 7, 1, 2)
            //Array(5, 7, 9, 11, 2, 3, 4)

            //println(s"unsorted was:${unsorted.take(10).mkString( """,""")}")
            /*too large for integer*/
            assume(
                    //true == true,
                    /*QuickSort(unsorted,
                                unsorted.length) == unsorted.sorted,*/
                    QuickSort(unsorted,
                              unsorted.length)
                    .sameElements(unsorted.sorted),
                    "must be equal"
                  )
          }
  ignore(
          "22: 'QuickSortComparisons' with first element as pivot" +
            "should return sorted Array & 'comparisonsTotal'"
        ) {
            val takeNumber =
              2000
            val unsorted: Array[Int] =
              getInput()
              .map(_.toInt)
              .toArray
            //Array(7, 5)
            //Array(5, 7, 1)
            //Array(5, 7, 1, 2)
            //Array(5, 7, 9, 11, 2, 3, 4)
            val expectedSorted: Array[Int] =
              unsorted
              //.take(takeNumber)
              .sorted
            val SortResults(sorted, comparisons): SortResults =
              QuickSortComparisons(
                                    unsorted,
                                    unsorted.length)
            println(s"'sorted' is:${sorted.take(10).mkString( """,""")}")
            println(s"'expectedSorted' is:${expectedSorted.take(10).mkString( """,""")}")
            println(s"'comparisons' is:${comparisons}")
            /*too large for integer*/
            assume(
                    //true == true,
                    sorted
                    .sameElements(expectedSorted),
                    "must be equal"
                  )
          }
  ignore(
          "23: 'QuickSortComparisons' with last element as pivot" +
            "should return sorted Array & 'comparisonsTotal'"
        ) {
            val takeNumber =
              20000
            val unsorted: Array[Int] =
              getInput()
              //.take(takeNumber)
              .map(_.toInt)
              .toArray
            //Array(7, 5)
            //Array(5, 7, 1)
            //Array(5, 7, 1, 2)
            //  Array(5, 7, 9, 11, 2, 3, 4)
            val expectedSorted: Array[Int] =
              unsorted
              //.take(takeNumber)
              .sorted
            val SortResults(sorted, comparisons): SortResults =
              QuickSortComparisons(
                                    unsorted,
                                    unsorted.length,
                                    pivotRule = LastPivot
                                  )
            println(s"\n'sorted' is:${
              sorted
              .take(10)
              .mkString( """,""")
            }")
            println(s"'expectedSorted' is:${
              expectedSorted
              .take(10)
              .mkString( """,""")
            }")
            println(s"'comparisons' is:${comparisons}")
            /*too large for integer*/
            assume(
                    //true == true,
                    sorted
                    .sameElements(expectedSorted),
                    "must be equal"
                  )
          }
  ignore(
          "24: 'QuickSortComparisons' using the \"median-of-three\" pivot " +
            "rule" +
            "should return sorted Array & 'comparisonsTotal'"
        ) {
            val takeNumber =
              2000
            val unsorted: Array[Int] =
              getInput()
              .take(takeNumber)
              .map(_.toInt)
              .toArray
            //Array(7, 5)
            //Array(5, 7, 1)
            //Array(5, 7, 1, 2)
            //  Array(5, 7, 9, 11, 2, 3, 4)
            val expectedSorted: Array[Int] =
              unsorted
              .take(takeNumber)
              .sorted
            val SortResults(sorted, comparisons): SortResults =
              QuickSortComparisons(
                                    unsorted,
                                    unsorted.length,
                                    pivotRule = MedianPivot
                                  )
            println(s"\n'sorted' is:${
              sorted
              .take(10)
              .mkString( """,""")
            }")
            println(s"'expectedSorted' is:${
              expectedSorted
              .take(10)
              .mkString( """,""")
            }")
            println(s"'comparisons' is:${comparisons}")
            /*too large for integer*/
            assume(
                    //true == true,
                    sorted
                    .sameElements(expectedSorted),
                    "must be equal"
                  )
          }
  ignore(
          "31: 'PivotingArrayToEmptyClone'" +
            "should return pivoted Array"
        ) {
            //val inversionsNumber = new InversionsNumber
            val unsorted: Array[Int] =
            /*getInput()
            .map(_.toInt)
            .toArray*/
            //Array(7, 5)
            //Array(5, 7, 1)
            //Array(5, 7, 1, 2)
              Array(5, 7, 9, 11, 2, 3, 4)

            println(s"'unsorted' was:${
              unsorted.take(10).mkString( """,""")
            }")
            println(s"'pivot' is:${
              unsorted.last
            }")
            val (part1, part2) =
              unsorted
              .init
              .partition(_ <= unsorted.last)
            val expectedArray: Array[Int] =
              part1 /*.tail*/ ++ (unsorted.last +: part2)
            val pivotedArray: Array[Int] =
              PivotingArrayToEmptyClone(
                                         unsorted,
                                         unsorted.length,
                                         pivotIndex = unsorted.length - 1
                                       )

            println(s"'pivotedArray' is:${
              pivotedArray
              .take(10)
              .mkString(
                  ""","""
                       )
            }")
            println(s"'expectedArray' is:${
              expectedArray.take(10).mkString( """,""")
            }")
            /*too large for integer*/
            assume(
                    //true == true,
                    pivotedArray
                    .sameElements(expectedArray),
                    "must be equal"
                  )
          }
  ignore(
        "32: 'PivotingArrayInPlace'" +
          "with 'FirstPivot'" +
          "should return pivoted Array & " +
          "'pivotIndex' for pivot in 'right' place"
      ) {
          //val inversionsNumber = new InversionsNumber
          val unsorted: Array[Int] =
          /*getInput()
          .map(_.toInt)
          .toArray*/
          //Array(7, 5)
            Array(5, 7, 1)
          //Array(5, 7, 1, 2)
          //  Array(5, 7, 9, 11, 2, 3, 4)

          println(s"\n'unsorted' was:${
            unsorted.take(10).mkString( """,""")
          }")
          println(s"'FirstPivot' is:${
            unsorted.head
          }")
          val (part1, part2) =
          /*unsorted
          .init
          .partition(_ <= unsorted.last)*/
            pivotParts(
                        sourceArray = unsorted,
                        pivotIndex = 0
                      )
          val expectedArray: Array[Int] =
            part1 /*.tail*/ ++ (unsorted.head +: part2)
          //val pivotedArray: Array[Int] =
          //val PivotingResults(pivotedArray, pivotIndex): PivotingResults =
          val pivotingResults: PivotingResults =
            PivotingArrayInPlace(
                                  sourceArray = unsorted,
                                  startingIndex = 0,
                                  endingIndex = unsorted.length - 1,
                                  pivotIndex =
                                    0
                                  /*ChooseFirstElementAsPivot(
                                                            unsorted,
                                                            unsorted.length,
                                                            0,
                                                            unsorted.length
                                                              - 1
                                                          )*/
                                )

          println(s"new 'pivotIndex' is:${
            pivotingResults
            .pivotIndex
          }")
          println(s"'pivotedArray' is:${
            pivotingResults
            .sortedArray
            //.pivotedArray
            .take(10)
            .mkString(
                ""","""
                     )
          }")
          println(s"'expectedArray' is:${
            expectedArray.take(10).mkString( """,""")
          }")
          /*too large for integer*/
          assume(
                  //true == true,
                  pivotingResults
                  .sortedArray
                  //pivotedArray
                  .sameElements(expectedArray),
                  "must be equal"
                )
        }
  ignore(
        "33: 'PivotingArrayInPlace'" +
          "with 'LastPivot'" +
          "should return pivoted Array & " +
          "'pivotIndex' for pivot in 'right' place"
      ) {
          //val inversionsNumber = new InversionsNumber
          val unsorted: Array[Int] =
          /*getInput()
          .map(_.toInt)
          .toArray*/
          //Array(7, 5)
            Array(5, 7, 1)
          //Array(5, 7, 1, 2)
          //  Array(5, 7, 9, 11, 2, 3, 4)

          println(s"\n'unsorted' was:${
            unsorted.take(10).mkString( """,""")
          }")
          println(s"'LastPivot' is:${
            unsorted.last
          }")
          val (part1, part2) =
          /*unsorted
          .init
          .partition(_ <= unsorted.last)*/
            pivotParts(
                        sourceArray = unsorted,
                        pivotIndex = unsorted.length - 1
                      )
          val expectedArray: Array[Int] =
            part1 /*.tail*/ ++ (unsorted.last +: part2)
          //val pivotedArray: Array[Int] =
          //val PivotingResults(pivotedArray, pivotIndex): PivotingResults =
          val pivotingResults: PivotingResults =
            PivotingArrayInPlace(
                                  sourceArray = unsorted,
                                  startingIndex = 0,
                                  endingIndex = unsorted.length - 1,
                                  pivotIndex =
                                    ChooseLastElementAsPivot(
                                                              unsorted,
                                                              unsorted.length,
                                                              0,
                                                              unsorted.length
                                                                - 1
                                                            )
                                )

          println(s"new 'pivotIndex' is:${
            pivotingResults
            .pivotIndex
          }")
          println(s"'pivotedArray' is:${
            pivotingResults
            .sortedArray
            //.pivotedArray
            .take(10)
            .mkString(
                ""","""
                     )
          }")
          println(s"'expectedArray' is:${
            expectedArray.take(10).mkString( """,""")
          }")
          /*too large for integer*/
          assume(
                  //true == true,
                  pivotingResults
                  .sortedArray
                  //pivotedArray
                  .sameElements(expectedArray),
                  "must be equal"
                )
        }
  ignore(
        "34: 'PivotingArrayInPlace'" +
          "with 'MedianPivot'" +
          "should return pivoted Array & " +
          "'pivotIndex' for pivot in 'right' place"
      ) {
          //val inversionsNumber = new InversionsNumber
          val unsorted: Array[Int] =
          /*getInput()
          .map(_.toInt)
          .toArray*/
          //Array(7, 5)
          //  Array(5, 7, 1)
          //  Array(3, 2, 4)
          //  Array(4, 2, 3)
          Array(5, 7, 1, 2)
          //  Array(5, 7, 9, 11, 2, 3, 4)

          println(s"\n'unsorted' was:${
            unsorted.take(10).mkString( """,""")
          }")

          val medianIndex: Int =
            ChooseMedianOfThreeAsPivot(
                                        unsorted,
                                        0,
                                        unsorted
                                        .length - 1
                                      )
          println(s"'MedianPivot' is:${
            unsorted(medianIndex)
          }")
          val (part1, part2) =
          /*unsorted
          .init
          .partition(_ <= unsorted.last)*/
            pivotParts(
                        sourceArray = unsorted,
                        pivotIndex = medianIndex
                      )
          val expectedArray: Array[Int] =
            part1 /*.tail*/ ++ (unsorted(medianIndex) +: part2)
          //val pivotedArray: Array[Int] =
          //val PivotingResults(pivotedArray, pivotIndex): PivotingResults =
          val pivotingResults: PivotingResults =
            PivotingArrayInPlace(
                                  sourceArray = unsorted,
                                  startingIndex = 0,
                                  endingIndex = unsorted.length - 1,
                                  pivotIndex =
                                    medianIndex
                                )

          println(s"new 'pivotIndex' is:${
            pivotingResults
            .pivotIndex
          }")
          println(s"'pivotedArray' is:${
            pivotingResults
            .sortedArray
            //.pivotedArray
            .take(10)
            .mkString(
                ""","""
                     )
          }")
          println(s"'expectedArray' is:${
            expectedArray.take(10).mkString( """,""")
          }")
          /*too large for integer*/
          assume(
                  //true == true,
                  pivotingResults
                  .sortedArray
                  //pivotedArray
                  .sameElements(expectedArray),
                  "must be equal"
                )
        }
  ignore(
        "41: 'ChooseMedianOfThreeAsPivot'" +
          "should return median of Array"
      ) {
          //0,(1,[2],3,4),5
          //(1) + (4-1)/2
          //0,1,(2,3,[4],5,6),7
          //(2) + (6-2)/2
          //([0])
          //(0) + (0-0)/2
          //([0],1)
          //(0) + (1-0)/2
          //0,1,2,3,([4],5),6,7
          //(4) + (5-4)/2
          val unsorted: Array[Int] =
          /*getInput()
          .map(_.toInt)
          .toArray*/
          //Array(7, 5)
            //Array(3, 2, 4)
          //Array(5, 7, 1)
          //Array(5, 7, 1, 2)
            Array(5, 7, 9, 11, 2, 3, 4)

          println(s"\n'unsorted' is:${
            unsorted.take(10).mkString( """,""")
          }")
          val pivotIndex: Int =
            ChooseMedianOfThreeAsPivot(
                                        sourceSeq = unsorted,
                                        firstSeqIndex = 4,
                                        lastSeqIndex = 6
                                      )
          val pivot: Int =
            unsorted(pivotIndex)
          println(s"expected 'pivot' is:${
            pivot
          }")
          assume(
                  //true == true,
                  pivot == 3,
                  "must be equal"
                )
        }
  ignore(
        "42: 'ChooseMedianOfThreeAsPivot' for two elements" +
          "should return median as min element"
      ) {
          //0,(1,[2],3,4),5
          //(1) + (4-1)/2
          //0,1,(2,3,[4],5,6),7
          //(2) + (6-2)/2
          //([0])
          //(0) + (0-0)/2
          //([0],1)
          //(0) + (1-0)/2
          //0,1,2,3,([4],5),6,7
          //(4) + (5-4)/2
          val unsorted: Array[Int] =
          /*getInput()
          .map(_.toInt)
          .toArray*/
            Array(7, 5)
          //Array(3, 2, 4)
          //Array(5, 7, 1)
          //Array(5, 7, 1, 2)
          //  Array(5, 7, 9, 11, 2, 3, 4)

          println(s"\n'unsorted' is:${
            unsorted.take(10).mkString( """,""")
          }")
          val pivotIndex: Int =
            ChooseMedianOfThreeAsPivot(
                                        sourceSeq = unsorted,
                                        firstSeqIndex = 0,
                                        lastSeqIndex =
                                          unsorted.length - 1
                                      )
          val pivot: Int =
            unsorted(pivotIndex)
          println(s"expected 'pivot' is:${
            pivot
          }")
          assume(
                  //true == true,
                  pivot == unsorted.min,
                  "must be equal"
                )
        }
  test(
          "51: 'QuickSortWithInPlacePivotingComparisons' " +
            "using the 'FirstPivot' rule" +
            "should return sorted Array & 'comparisonsTotal'"
        ) {
            val takeNumber =
              1000
            val unsorted: Array[Int] =
            getInput()
            //.take(takeNumber)
            .map(_.toInt)
            .toArray
            //Array(7, 5)
            //Array(5, 7, 1)
            //Array(5, 7, 1, 2)
              //Array(5, 7, 9, 11, 2, 3, 4)
              //Array(5, 7, 9, 11, 2, 3, 4, 1)
            val expectedSorted: Array[Int] =
              unsorted
              //.take(takeNumber)
              .sorted
            println(s"\n'expectedSorted' is:\n${
              expectedSorted
              .take(20)
              .mkString( """,""")
            }")
            val SortResults(sorted, comparisons): SortResults =
              QuickSortWithInPlacePivotingComparisons(
                                                       unsorted,
                                                       startIndex = 0,
                                                       endIndex = unsorted
                                                                  .length - 1,
                                                       pivotRule = FirstPivot
                                                     )
            println(s"\n'sorted' is:\n${
              sorted
              .take(20)
              .mkString( """,""")
            }")
            println(s"'comparisons' is:${comparisons}")
            /*too large for integer*/
            assume(
                    //true == true,
                    sorted
                    .sameElements(expectedSorted),
                    "must be equal"
                  )
          }
  test(
          "52: 'QuickSortWithInPlacePivotingComparisons' " +
            "using the LastPivot rule" +
            "should return sorted Array & 'comparisonsTotal'"
        ) {
            val takeNumber =
              1000
            val unsorted: Array[Int] =
            getInput()
            //.take(takeNumber)
            .map(_.toInt)
            .toArray
            //Array(7, 5)
            //Array(5, 7, 1)
            //Array(5, 7, 1, 2)
              //Array(5, 7, 9, 11, 2, 3, 4)
              //Array(5, 7, 9, 11, 2, 3, 4, 1)
            val expectedSorted: Array[Int] =
              unsorted
              //.take(takeNumber)
              .sorted
            println(s"\n'expectedSorted' is:\n${
              expectedSorted
              .take(20)
              .mkString( """,""")
            }")
            val SortResults(sorted, comparisons): SortResults =
              QuickSortWithInPlacePivotingComparisons(
                                                       unsorted,
                                                       startIndex = 0,
                                                       endIndex = unsorted
                                                                  .length - 1,
                                                       pivotRule = LastPivot
                                                     )
            println(s"\n'sorted' is:\n${
              sorted
              .take(20)
              .mkString( """,""")
            }")
            println(s"'comparisons' is:${comparisons}")
            /*too large for integer*/
            assume(
                    //true == true,
                    sorted
                    .sameElements(expectedSorted),
                    "must be equal"
                  )
          }
  test(
          "53: 'QuickSortWithInPlacePivotingComparisons' " +
            "using the \"median-of-three\" pivot rule" +
            "should return sorted Array & 'comparisonsTotal'"
        ) {
            val takeNumber =
              1000
            val unsorted: Array[Int] =
            getInput()
            //.take(takeNumber)
            .map(_.toInt)
            .toArray
            //Array(7, 5)
            //Array(5, 7, 1)
            //Array(5, 7, 1, 2)
              //Array(5, 7, 9, 11, 2, 3, 4)
              //Array(5, 7, 9, 11, 2, 3, 4, 1)
            val expectedSorted: Array[Int] =
              unsorted
              //.take(takeNumber)
              .sorted
            println(s"\n'expectedSorted' is:\n${
              expectedSorted
              .take(20)
              .mkString( """,""")
            }")
            val SortResults(sorted, comparisons): SortResults =
              QuickSortWithInPlacePivotingComparisons(
                                                       unsorted,
                                                       startIndex = 0,
                                                       endIndex = unsorted
                                                                  .length - 1,
                                                       pivotRule = MedianPivot
                                                     )
            println(s"\n'sorted' is:\n${
              sorted
              .take(20)
              .mkString( """,""")
            }")
            println(s"'comparisons' is:${comparisons}")
            /*too large for integer*/
            assume(
                    //true == true,
                    sorted
                    .sameElements(expectedSorted),
                    "must be equal"
                  )
          }

}
