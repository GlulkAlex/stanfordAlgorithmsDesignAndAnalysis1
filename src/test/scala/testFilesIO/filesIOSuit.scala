package testFilesIO

import filesIO.FilesIO._
import org.scalatest.FunSuite

/**
 * Created by gluk-alex on 7/23/15.
 */
class filesIOSuit
  extends FunSuite {
  ignore(
          "1: 'readFromFile'" +
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
  test(
        "11: 'writeToTextFile'" +
          "should write to text file"
      ) {
          val takeNumber: Int = 10
          val filePath: String =
            "/media/gluk-alex/" +
              "GDI/Java/Scala/sbt/projects/" +
              "stanfordAlgorithmsDesignAndAnalysis1/"
          val fileName: String = "test.txt"

          writeToTextFile()

          val fileContent: Array[String] =
            readFromFile(
                          fileName = fileName,
                          filePath = ""//filePath
                        )
            //.map(_.toInt)
            .toArray
          //Array(7, 5)
          //Array(5, 7, 1)
          //Array(5, 7, 1, 2)
          //Array(5, 7, 9, 11, 2, 3, 4)

          println(
                   s"\nfirst $takeNumber lines from 'fileContent' was:${
                     fileContent.take(10).mkString( """,""")
                   }")
          /*too large for integer*/
          assume(
                  //true == true,
                  fileContent.length == 1,
                  "must be equal to '1' line"
                )
        }
  test(
        "12: 'writeToTextFile2'" +
          "should write to text file"
      ) {
          val takeNumber: Int = 10
          val filePath: String =
            "/media/gluk-alex/" +
              "GDI/Java/Scala/sbt/projects/" +
              "stanfordAlgorithmsDesignAndAnalysis1/"
          val fileName: String = "test2.txt"
          writeToTextFile2()

          val fileContent: Array[String] =
            readFromFile(
                          fileName = fileName,
                          filePath = filePath
                        )
            //.map(_.toInt)
            .toArray

          println(
                   s"\nfirst $takeNumber lines from '$fileName' is:${
                     fileContent.take(10).mkString( """,""")
                   }")
          assume(
                  //true == true,
                  fileContent.length == 10,
                  "must be equal to '10' line"
                )
        }
  test(
        "13: 'writeToTextFile3'" +
          "should write to text file"
      ) {
          val takeNumber: Int = 10
          val filePath: String =
            "/media/gluk-alex/" +
              "GDI/Java/Scala/sbt/projects/" +
              "stanfordAlgorithmsDesignAndAnalysis1/"
          val fileName: String = "test3.txt"

          writeToTextFile3()

          val fileContent: Array[String] =
            readFromFile(
                          fileName = fileName,
                          filePath = filePath
                        )
            //.map(_.toInt)
            .toArray

          println(
                   s"\nfirst $takeNumber lines from '$fileName' is:${
                     fileContent.take(10).mkString( """,""")
                   }")
          assume(
                  //true == true,
                  fileContent.length == 10,
                  "must be equal to '10' line"
                )
        }

}
