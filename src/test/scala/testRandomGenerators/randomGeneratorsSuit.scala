package testRandomGenerators

import filesIO.FilesIO._
import randomGenerators.RandomGenerators._
import org.scalatest.FunSuite

/**
 * Created by gluk-alex on 7/23/15.
 */
class randomGeneratorsSuit
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
  ignore(
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
                            filePath = "" //filePath
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
  ignore(
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
  ignore(
          "13: 'writeJSON_ToFile'" +
            "should write to text file"
        ) {
            val takeNumber: Int = 10
            /*
            /media/gluk-alex/
            GDI/
            Java/Scala/sbt/projects/
            stanfordAlgorithmsDesignAndAnalysis1/src/
            test/scala/
            testClosestPoints/ClosestPointsSuit.scala
             */
            val filePath: String =
              "/media/gluk-alex/" +
                "GDI/Java/Scala/sbt/projects/" +
                "stanfordAlgorithmsDesignAndAnalysis1/" +
                "src/test/scala/testClosestPoints/"
            val fileName: String = "JSON.txt"
            val lowerBound: Int = 0
            val upperBound: Int = 1000

            val fileContent: Array[(Double, Double)] =
              (for (i <- 1 to takeNumber) yield {
                val xCoordinate: Double =
                  intervalImproved(
                                    lo = lowerBound,
                                    hi = upperBound).generate +
                    //doubles
                    doublesTruncated(2)
                    .generate
                val yCoordinate: Double =
                  intervalImproved(
                                    lo = lowerBound,
                                    hi = upperBound).generate +
                    //doubles
                    doublesTruncated(2)
                    .generate
                /*return value*/
                (xCoordinate, yCoordinate)
              }).toArray

            writeJSON_ToFile(
                              filePath = filePath,
                              fileName = fileName,
                              fileContent = fileContent
                            )

            val actualFileContent: Array[String] =
              readFromFile(
                            fileName = fileName,
                            filePath = filePath
                          )
              //.map(_.toInt)
              .toArray

            println(
                     s"\nfirst $takeNumber lines from '$fileName' is:\n${
                       actualFileContent.take(10).mkString("\n")
                     }")
            assume(
                    //true == true,
                    fileContent.length + 2 == actualFileContent.length,
                    s"'length' must be equal"
                  )
          }
  ignore(
          "21: 'generators'" +
            "should generate double from specified interval"
        ) {
            val lowerBound: Int = 0
            val upperBound: Int = 1000
            val coordinate: Double =
            //interval(
              intervalImproved(
                                lo = lowerBound,
                                hi = upperBound).generate +
                //doubles
                doublesTruncated(2)
                .generate

            println(
                     s"\n'$lowerBound' <= '$coordinate' < '${
                       upperBound
                     }'")
            assume(
                    //true == true,
                    coordinate >= 0 && coordinate < upperBound + 1,
                    s"must be equal or greater than '0' & less then " +
                      s"'$upperBound'"
                  )
          }
  test(
        "31: 'randomlyOrderedSequenceFromInterval'" +
          "should generate randomly ordered sequence from specified interval"
      ) {
          val takeNumber: Int = 15
          val lowerBound: Int = 0
          val upperBound: Int = 15
          val randomIntSeq: Seq[Int] =
            randomlyOrderedSequenceFromInterval(
                                                 lowerBound,
                                                 upperBound)

          println(
                   s"\n'lowerBound' is: $lowerBound\n'upperBound' is:${
                     upperBound
                   }")
          println(
                   s"\n'randomIntSeq' is:'\n${
                     randomIntSeq
                     .take(takeNumber)
                     .mkString("{", "|", "}")
                   }'")
          println(
                   s"\n'randomIntSeq' is:'\n${
                     randomIntSeq
                     .sorted
                     .take(takeNumber)
                     .mkString("{", "|", "}")
                   }'")
          assume(
                  //true == true,
                  randomIntSeq.length == upperBound + 1 - lowerBound,
                  s"'randomIntSeq.length' must be " +
                    s"equal to interval size "
                )
        }

}
