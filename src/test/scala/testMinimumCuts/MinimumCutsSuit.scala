package testMinimumCuts

import org.scalatest.FunSuite
import minCutRandomContractionPQ3.MinimumCuts._
import filesIO.FilesIO._
/**
 * Created by gluk-alex on 7/23/15.
 */
class MinimumCutsSuit
  extends FunSuite {
  ignore(
          "1: 'getInput'" +
            "should read from file"
        ) {
            //val inversionsNumber = new InversionsNumber
            val unsorted: Array[Int] =
              readFromFile()
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

}
