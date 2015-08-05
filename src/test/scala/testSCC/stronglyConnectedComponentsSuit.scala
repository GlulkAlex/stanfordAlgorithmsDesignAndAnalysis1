package testSCC

import filesIO.FilesIO._

//import minCutRandomContractionPQ3.MinimumCuts._

import stronglyConnectedComponentsPQ4.stronglyConnectedComponents._
import org.scalatest.FunSuite

/**
 * Created by gluk-alex on 7/23/15.
 */
class stronglyConnectedComponentsSuit
  extends FunSuite {
  ignore(
          "1: 'extractGraphComponents'" +
            "should extract Int values from source"
        ) {
            val takeNumber: Int = 3
            val sourceSize: Int = 200
            val filePath: String =
              "/media/gluk-alex/" +
                "GDI/Java/Scala/sbt/projects/" +
                "stanfordAlgorithmsDesignAndAnalysis1/" +
                "src/test/scala/" +
                "testSCC/"
            //SCC.txt
            val fileName: String = "SCC.txt"
            val actualFileContent: Iterator[String] =
              readFromFile(
                            fileName = fileName,
                            filePath = filePath
                          )
            val graphVector: Vector[String] =
              actualFileContent
              .toVector

            /*println(
                     s"\nfirst $takeNumber lines from '$fileName' is:\n${
                       actualFileContent.take(5).mkString("\n")
                     }")*/
            println(
                     s"\n'graphVector.size' is:\n${
                       graphVector.length
                     } entries")
            println(
                     s"\nfirst $takeNumber entries from '$fileName' is:\n${
                       graphVector.take(takeNumber).mkString("\n")
                     }")
            println(
                     s"\nlast $takeNumber entries from '$fileName' is:\n${
                       graphVector.takeRight(takeNumber).mkString("\n")
                     }")
            assume(
                    //true == true,
                    graphVector.length >= 0,
                    "'graphVector' must be non empty"
                  )
          }
  ignore(
        "2: 'extractArcs'" +
          "should extract all Arcs from source"
      ) {
          val takeNumber: Int = 75
          val sourceSize: Int = 5105043
          val filePath: String =
            "/media/gluk-alex/" +
              "GDI/Java/Scala/sbt/projects/" +
              "stanfordAlgorithmsDesignAndAnalysis1/" +
              "src/test/scala/" +
              "testSCC/"
          //SCC.txt
          val fileName: String = "SCC.txt"
          val actualFileContent: Iterator[String] =
            readFromFile(
                          fileName = fileName,
                          filePath = filePath
                        )
          val arcs: Vector[Arc] =
            extractArcs(actualFileContent)

          println(
                   s"\nfirst `arc` from '$fileName' is:\n${
                     arcs.head
                   }")
          println(
                   s"\nlast `arc` from '$fileName' is:\n${
                     arcs.last
                   }")
          assume(
                  //true == true,
                  arcs.length == sourceSize,
                  "'arcs.length' must be equal to 'sourceSize'"
                )
        }
  test(
        "3: 'BFS'" +
          "should return connected component"
      ) {
          val takeNumber: Int = 7
          val sourceSize: Int = 5105043
          val expectedSize: Int = 3
          val mockUpGraph: Vector[Arc] =
          Vector(
          Arc(1,2),
          Arc(1,3),
          //Arc(2,5),
          //Arc(3,5),
          //Arc(3,4),
          Arc(4,5),
          Arc(4,6),
          Arc(5,6)
                )
          val connectedComponent: Vector[Int] =
            BFS(
                 graph=mockUpGraph,
                 startingNode = 4,
                 /*eventually must reduce to empty*/
                 scala.collection.immutable.Queue[Arc]()
                 .enqueue(Arc(4,5)),
                 //Vector(1)
                 Vector.empty[Int]
               )

          println(
                   s"\nfirst $takeNumber`nodes` from 'connectedComponent' are:\n${
                     connectedComponent.mkString("\n")
                   }")
          assume(
                  //true == true,
                  connectedComponent.length == expectedSize,
                  "'connectedComponent.length' must be equal to 'expectedSize'"
                )
        }

}
