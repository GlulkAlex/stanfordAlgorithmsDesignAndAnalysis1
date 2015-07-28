package testClosestPoints

import filesIO.FilesIO._
import org.scalatest.FunSuite

/**
 * Created by gluk-alex on 7/23/15.
 */
class ClosestPointsSuit
  extends FunSuite {
  test(
          "1: 'readFromFile'" +
            "should read from file"
        ) {
            val takeNumber: Int = 15
            val filePath: String =
              "/media/gluk-alex/" +
                "GDI/Java/Scala/sbt/projects/" +
                "stanfordAlgorithmsDesignAndAnalysis1/" +
                "src/test/scala/testClosestPoints/"
            val fileName: String = "JSON.txt"
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
                    actualFileContent.length > 0,
                    "must be equal"
                  )
          }
}
