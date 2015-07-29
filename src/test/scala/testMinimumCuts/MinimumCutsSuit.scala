package testMinimumCuts

import org.scalatest.FunSuite
import minCutRandomContractionPQ3.MinimumCuts._
import randomGenerators.RandomGenerators._
import filesIO.FilesIO._

/**
 * Created by gluk-alex on 7/23/15.
 */
class MinimumCutsSuit
  extends FunSuite {
  ignore(
          "1: 'extractGraphComponents'" +
            "should extract Int values from source"
        ) {
            val takeNumber: Int = 15
            val sourceSize: Int = 200
            val filePath: String =
              "/media/gluk-alex/" +
                "GDI/Java/Scala/sbt/projects/" +
                "stanfordAlgorithmsDesignAndAnalysis1/" +
                "src/test/scala/" +
                //"testClosestPoints/"
    "testMinimumCuts/" //+
                //"kargerMinCut.txt"
            val fileName: String = "kargerMinCut.txt"
            val actualFileContent: Iterator[String] =
              readFromFile(
                            fileName = fileName,
                            filePath = filePath
                          )
            val adjacencyVector: Vector[VvsE] =
              extractGraphComponents(actualFileContent)

            println(
                     s"\nfirst $takeNumber lines from '$fileName' is:\n${
                       actualFileContent.take(5).mkString("\n")
                     }")
            println(
                     s"\nfirst $takeNumber points from '$fileName' is:\n${
                       adjacencyVector.take(5).mkString("\n")
                     }")
            assume(
                    //true == true,
                    adjacencyVector.length == sourceSize,
                    "'adjacencyVector' must be non empty"
                  )
          }
  test(
          "2: 'extractEdges'" +
            "should extract Edges from source" +
  "but how to check that is done correct"
        ) {
            val takeNumber: Int = 75
            val sourceSize: Int = 200
            val filePath: String =
              "/media/gluk-alex/" +
                "GDI/Java/Scala/sbt/projects/" +
                "stanfordAlgorithmsDesignAndAnalysis1/" +
                "src/test/scala/" +
                //"testClosestPoints/"
    "testMinimumCuts/" //+
                //"kargerMinCut.txt"
            val fileName: String = "kargerMinCut.txt"
            val actualFileContent: Iterator[String] =
              readFromFile(
                            fileName = fileName,
                            filePath = filePath
                          )
            val edges: Vector[Edge] =
              extractEdges(actualFileContent)

            /*println(
                     s"\nfirst $takeNumber lines from '$fileName' is:\n${
                       actualFileContent.take(5).mkString("\n")
                     }")*/
            println(
                     s"\nfirst $takeNumber edges from '$fileName' is:\n${
                       edges.take(takeNumber).mkString("\n")
                     }")
            assume(
                    //true == true,
                    edges.length > 0,
                    "'edges' must be non empty"
                  )
          }

}
