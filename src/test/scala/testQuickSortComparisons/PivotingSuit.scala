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
          "22: 'QuickSortComparisons'" +
            "should return sorted Array & 'comparisonsTotal'"
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

            val (sorted, comparisons): (Array[Int], Int) =
              QuickSortComparisons(
                                    unsorted,
                                    unsorted.length)
            println(s"'sorted' is:${sorted.take(10).mkString( """,""")}")
            println(s"'comparisons' is:${comparisons}")
            /*too large for integer*/
            assume(
                    //true == true,
                    sorted
                    .sameElements(unsorted.sorted),
                    "must be equal"
                  )
          }
  test(
          "23: 'QuickSortComparisons' with last element as pivot" +
            "should return sorted Array & 'comparisonsTotal'"
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

            val (sorted, comparisons): (Array[Int], Int) =
              QuickSortComparisons(
                                    unsorted,
                                    unsorted.length)
            println(s"'sorted' is:${sorted.take(10).mkString( """,""")}")
            println(s"'comparisons' is:${comparisons}")
            /*too large for integer*/
            assume(
                    //true == true,
                    sorted
                    .sameElements(unsorted.sorted),
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
            PivotingArrayInPlace(
                                  sourceArray = unsorted,
                                  startingIndex = 0,
                                  endingIndex = unsorted.length - 1,
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

}
