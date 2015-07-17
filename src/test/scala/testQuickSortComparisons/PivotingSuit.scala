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
  test(
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
  test(
        "21: 'QuickSort'" +
          "should return sorted Array"
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
                  /*QuickSort(unsorted,
                              unsorted.length) == unsorted.sorted,*/
                  QuickSort(unsorted,
                            unsorted.length)
                  .sameElements(unsorted.sorted),
                  "must be equal"
                )
        }

}
