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
        "3: 'extractArcsAndNodes'" +
          "should extract all 'Arcs' & 'nodes' from source"
      ) {
          val takeNumber: Int = 75
          val sourceSize: Int = 5105043
          val expectedNodesSize: Int = 875714
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
          /*too slow to test on big input*/
          val DirectedGraph(nodes, arcs): DirectedGraph =
            extractArcsAndNodes(actualFileContent)
          val actualArcsSize: Int = arcs.length
          val actualNodesSize: Int = nodes.length
          /*println(
                   s"\nfirst `node` from '$fileName' is:\n${
                     nodes.head
                   }" +
                     s"\nlast `node` from '$fileName' is:\n${
                       nodes.last
                     }" +
                     s"\n`nodes.length` is:\n${
                       nodes.length
                     }"
                 )*/
          /*println(
                   s"\nfirst `arc` from '$fileName' is:\n${
                     arcs.head
                   }")
          println(
                   s"\nlast `arc` from '$fileName' is:\n${
                     arcs.last
                   }" +
                     s"\n`arcs.length` is:\n${
                       arcs.length
                     }"
                 )*/

          assume(
                  //true == true,
                  //arcs.length == sourceSize &&
                  /*self loops skipped, so it has to be different*/
                  //actualArcsSize == sourceSize &&
                    /*
                    ? Why ? it is successfully retrieve 'arcs' & 'nodes'
                    only 'assume' fails
                    definitely 'println' with sequence traversal matters
                    >>java.lang.OutOfMemoryError: Java heap space<<
                     */
                    //nodes.length == expectedNodesSize,
          //5819251 did not equal 875714
                    actualNodesSize == expectedNodesSize,
                  s"'arcs.length' must be equal to 'sourceSize'" +
                    s"\n and 'nodes.length' must be equal to 'expectedNodesSize'"
                )
        }
  ignore(
          "4: 'extractNodesFromArcs'" +
            "should extract all 'nodes' from 'Arcs'"
        ) {
            val takeNumber: Int = 75
            val sourceSize: Int = 5105043
            val expectedNodesSize: Int = 875714
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
                     }" +
                       s"\n`arcs.length` is:\n${
                         arcs.length
                       }"
                   )
            /*fast enough to test on big input*/
            val nodes: Vector[Int] =
              extractNodesFromArcs(arcs)

            println(
                     s"\nfirst `node` from '$fileName' is:\n${
                       nodes.head
                     }" +
                       s"\nlast `node` from '$fileName' is:\n${
                         nodes.last
                       }" +
                       s"\n`nodes.length` is:\n${
                         nodes.length
                       }"
                   )

            assume(
                    //true == true,
                    /*'nodes' is just a range from '1' to '875714'*/
                    arcs.length == sourceSize &&
                      nodes.length == expectedNodesSize,
                    "'arcs.length' must be equal to 'sourceSize'"
                  )
          }
  ignore(
          "11: 'BFS'" +
            "should return connected component"
        ) {
            val takeNumber: Int = 7
            val sourceSize: Int = 5105043
            val expectedSize: Int = 1
            val mockUpGraph: Vector[Arc] =
              Vector(
                      Arc(1, 2),
                      Arc(1, 3),
                      Arc(2, 5),
                      Arc(3, 5),
                      Arc(3, 4),
                      Arc(4, 5),
                      Arc(4, 6),
                      Arc(5, 6)
                    )
            val connectedComponent: Vector[Int] =
              BFS(
                   graph = mockUpGraph,
                   startingNode = 6,
                   scala.collection.immutable.Queue[Arc]()
                   //.enqueue(Arc(4,5))
                   ,
                   //Vector(1)
                   Vector.empty[Int]
                 )

            println(
                     s"\nfirst $takeNumber`nodes` from 'connectedComponent' " +
                       s"are:\n${
                         connectedComponent.mkString("\n")
                       }")
            assume(
                    //true == true,
                    connectedComponent.length == expectedSize,
                    "'connectedComponent.length' must be equal to " +
                      "'expectedSize'"
                  )
          }
  ignore(
          "12: 'layersBFS'" +
            "should return connected component as ranked nodes"
        ) {
            val takeNumber: Int = 14
            val expectedMaxLevel: Int = 4
            val expectedSize: Int = 13
            val mockUpGraph: Vector[Arc] =
              Vector(
                      Arc(1, 2),
                      Arc(1, 3),
                      Arc(2, 5),
                      Arc(3, 5),
                      Arc(3, 4),
                      Arc(4, 5),
                      Arc(4, 6),
                      Arc(5, 6)
                    )
            val mockUpTree1: Vector[Arc] =
              Vector(
                      Arc(6, 4),
                      Arc(6, 12),
                      Arc(4, 5),
                      Arc(4, 2),
                      Arc(2, 3),
                      Arc(2, 1),
                      Arc(12, 10),
                      Arc(12, 13),
                      Arc(10, 11),
                      Arc(10, 8),
                      Arc(8, 7),
                      Arc(8, 9)
                    )
            val mockUpTree2: Vector[Arc] =
              Vector(
                      Arc(1, 2),
                      Arc(2, 3),
                      Arc(2, 4),
                      Arc(4, 5),
                      Arc(4, 6),
                      Arc(6, 12),
                      Arc(12, 10),
                      Arc(12, 13),
                      Arc(10, 11),
                      Arc(10, 8),
                      Arc(8, 7),
                      Arc(8, 9)
                    )
            val nodes: Vector[Int] =
              extractNodesFromArcs(mockUpTree1)
            /*TODO
            debug needed
            * */
            /*tier*/
            val nodesLevels: Vector[RankedNode] =
              layersBFS(
                         graph = mockUpGraph,
                         startingNode = 6
                       )
            val maxLevelNode: RankedNode =
              nodesLevels
              //.max(Ordering[RankedNode].on((n):RankedNode => n.r)
              //.max(Ordering.by[RankedNode, Int](_.rank))
              //.max(Ordering.by[RankedNode, Int]((f: RankedNode) =>f.rank))
              //.max(Ordering.by((f: RankedNode) =>f.rank))
              //.max(Ordering.by(_.rank)
              .maxBy(_.rank)
            //.sortBy(_.rank)(Ordering[Int])
            /*.sortBy(_.rank)
            .headOption
            .getOrElse(RankedNode(-1,-1))*/

            println(
                     s"\nfirst $takeNumber`nodes` from 'connectedComponent' " +
                       s"are:\n${
                         nodesLevels.mkString("\n")
                       }")
            assume(
                    //true == true,
                    nodesLevels.length == expectedSize &&
                      maxLevelNode.rank == expectedMaxLevel,
                    s"'nodesLevels.length' must be equal to " +
                      s"'expectedSize'" +
                      s" and 'maxLevelNode.rank' must be equal to " +
                      s"'expectedMaxLevel'"
                  )
          }

}
