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
  ignore(
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
            val (edges, nodes) =
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
                    edges.length > 0 &&
                      nodes.length > 0,
                    "'edges' must be non empty"
                  )
          }
  ignore(
          "3: 'randomizedEdgeContraction'" +
            "should return number of Edges" +
            "between two last nodes"
        ) {
            val takeNumber: Int = 5
            //val sourceSize: Int = 200
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
            val (edges, nodes) =
              extractEdges(actualFileContent)
            val mockNodes: Vector[Int] =
              (1 to 3).toVector
            val mockNodes2: Vector[Int] =
              (1 to 8).toVector
            val mockEdges: Vector[Edge] =
              Vector(
                      Edge(1, 2),
                      Edge(1, 3),
                      Edge(2, 3)
                    )
            val mockEdges2: Vector[Edge] =
              Vector(
                      Edge(1, 2),
                      Edge(1, 8),
                      Edge(1, 7),
                      Edge(2, 7),
                      Edge(2, 8),
                      Edge(2, 3),
                      Edge(3, 4),
                      Edge(3, 5),
                      Edge(3, 6),
                      Edge(4, 5),
                      Edge(4, 6),
                      Edge(5, 6),
                      Edge(6, 7),
                      Edge(7, 8)
                    )
            val lastCut: Int =
              randomizedEdgeContraction(
                                         //nodes
                                         mockNodes
                                         .length,
                                         //edges
                                         mockEdges
                                       )
            val expectedValueMin: Int = 2
            //val expectedValueMax: Int = 3

            /*println(
                     s"\nfirst $takeNumber lines from '$fileName' is:\n${
                       actualFileContent.take(5).mkString("\n")
                     }")*/
            println(
                     s"\n'lastCut' is:${
                       lastCut
                     }")
            assume(
                    //true == true,
                    lastCut >= expectedValueMin /*&&
                    lastCut <= expectedValueMax*/ ,
                    "'lastCut' must be > '0'"
                  )
          }
  test(
        "4: 'minimumCutTrails'" +
          "should return number of " +
          "min cuts"
      ) {
          val takeNumber: Int = 5
          //val sourceSize: Int = 200
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
          val (edges, nodes) =
            extractEdges(actualFileContent)
          val mockNodes: Vector[Int] =
            (1 to 8).toVector
          val mockEdges: Vector[Edge] =
            Vector(
                    Edge(1, 2),
                    Edge(1, 8),
                    Edge(1, 7),
                    Edge(2, 7),
                    Edge(2, 8),
                    Edge(2, 3),
                    Edge(3, 4),
                    Edge(3, 5),
                    Edge(3, 6),
                    Edge(4, 5),
                    Edge(4, 6),
                    Edge(5, 6),
                    Edge(6, 7),
                    Edge(7, 8)
                  )
          val tab = "" + '\u0009'
          val mockAdjacencyList: String =
            List(1, 2, 7, 8).mkString("", tab, "\n") +
              List(2, 1, 8, 7, 3).mkString("", tab, "\n") +
              List(3, 2, 4, 5, 6).mkString("", tab, "\n") +
              List(4, 3, 6, 5).mkString("", tab, "\n") +
              List(5, 3, 4, 6).mkString("", tab, "\n") +
              List(6, 3, 4, 5, 7).mkString("", tab, "\n") +
              List(7, 1, 2, 6, 8).mkString("", tab, "\n") +
              List(8, 1, 2, 7).mkString("", tab, "")
          val trailsTotal: Int =
            trailsNumber(nVertices =
                           //8
            nodes.length
                        )
          val minCut: Int =
            minimumCutTrails(
                              //mockAdjacencyList.lines
                                actualFileContent
                            )
          val expectedValueMin: Int = 2
          //val expectedValueMax: Int = 3

          /*println(
                   s"\nfirst $takeNumber lines from '$fileName' is:\n${
                     actualFileContent.take(5).mkString("\n")
                   }")*/
          println(
                   s"\n'trailsTotal' is:${
                     trailsTotal
                   }")
          println(
                   s"\n'minCut' is:${
                     minCut
                   }")
          assume(
                  //true == true,
                  minCut == expectedValueMin /*&&
                    lastCut <= expectedValueMax*/ ,
                  s"'lastCut' must be equal to '$expectedValueMin'"
                )
        }

}
