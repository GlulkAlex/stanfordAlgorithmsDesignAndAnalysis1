package testSCC

import java.text.SimpleDateFormat
import java.util.Calendar

import filesIO.FilesIO._

import scala.collection.BitSet
import scala.util.matching.Regex

//import minCutRandomContractionPQ3.MinimumCuts._

import stronglyConnectedComponentsPQ4.stronglyConnectedComponents._
import stronglyConnectedComponentsPQ4.ShowProgress._
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
  ignore(
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
            println(
                     s"\nfirst `node` from '$fileName' is:\n${
                       nodes.head
                     }" +
                       s"\nlast `node` from '$fileName' is:\n${
                         nodes.last
                       }" +
                       s"\n`nodes.length` is:\n${
                         //nodes.length
                         actualNodesSize
                       }"
                   )
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
                      s"\n and 'nodes.length' must be equal to " +
                      s"'expectedNodesSize'"
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
            val expectedMaxLevel: Int = 3
            val expectedTotalLevels: Int = 4
            val expectedSize: Int =
              6
            //13
            val mockUpOneNode: Vector[Arc] =
              Vector(
                      Arc(1, 1)
                    )
            val mockUpTwoNodes: Vector[Arc] =
              Vector(
                      Arc(1, 2)
                    )
            val mockUpThreeNodes1: Vector[Arc] =
              Vector(
                      Arc(1, 2),
                      Arc(1, 3)
                    )
            val mockUpThreeNodes2: Vector[Arc] =
              Vector(
                      Arc(1, 2),
                      Arc(2, 3)
                    )
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
              extractNodesFromArcs(mockUpTree2)
            /*DONE
            debug needed
            * */
            /*tier*/
            val nodesLevels: Vector[RankedNode] =
              layersBFS(
                         graph = mockUpTree2,
                         startingNode = 1
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
            /*must be only distinct 'nodes'*/
            val rankedUnziped: (Vector[Int], Vector[Int]) =
              nodesLevels
              .unzip(r => (r.node, r.rank))
            val rankedNodes =
              rankedUnziped._1
            val levelsNumbers =
              rankedUnziped._2
            val nodesLevelsLength = nodesLevels.length
            val nodesRanks = levelsNumbers.distinct //.length

            println(
                     s"\n'maxLevelNode` is:${maxLevelNode.rank}" +
                       s"\namount of 'nodes` in CC is:${nodesLevels.length} " +
                       s"\nfirst $takeNumber`nodes` from 'connectedComponent'" +
                       s" " +
                       s"are:\n${
                         nodesLevels.mkString("\n")
                       }")
            assume(
                    //true == true,
                    /*can not exceed total number of 'nodes'*/
                    nodesLevelsLength <= nodes.length &&
                      /*must be only distinct 'nodes'*/
                      nodesLevelsLength == rankedNodes.distinct.length &&
                      nodesRanks.length == expectedTotalLevels &&
                      maxLevelNode.rank == expectedMaxLevel,
                    s"\n'nodesLevels.length' must be less or equal to " +
                      s"'nodes.length':${nodes.length}" +
                      //s"'expectedSize'" +
                      s"\n'nodesLevels' must contain only distinct 'nodes'" +
                      s"\n'nodesRanks.length' must be equal to " +
                      s"'expectedTotalLevels'" +
                      s"\n and 'maxLevelNode.rank' must be equal to " +
                      s"'expectedMaxLevel'"
                  )
          }
  ignore(
          "13: 'BFS_SCC_NodesAmount'" +
            "should return exact amount of nodes in connected component"
        ) {
            val takeNumber: Int = 14
            //val expectedNodesInSCC: Int = 3
            val expectedSize: Int =
            //6
              13
            val mockUpOneNode: Vector[Arc] =
              Vector(
                      Arc(1, 1)
                    )
            val mockUpTwoNodes: Vector[Arc] =
              Vector(
                      Arc(1, 2)
                    )
            val mockUpThreeNodes1: Vector[Arc] =
              Vector(
                      Arc(1, 2),
                      Arc(1, 3)
                    )
            val mockUpThreeNodes2: Vector[Arc] =
              Vector(
                      Arc(1, 2),
                      Arc(2, 3)
                    )
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
            //val nodesInSCC: Int =
            val ComponentResult(nodesInSCC, _): ComponentResult =
              BFS_SCC_NodesAmount(
                                   graph = mockUpTree2,
                                   startingNode = 1
                                 )

            println(
                     s"\n'nodesInSCC` is:${nodesInSCC}")
            assume(
                    //true == true,
                    nodesInSCC == expectedSize,
                    s"\nnumber of 'nodesInSCC' must be equal to " +
                      s"'expectedSize'"
                  )
          }
  ignore(
          "14: 'BFS_SCC_NodesAmountImproved'" +
            "should " +
            "return exact amount of nodes in connected component"
        ) {
            val takeNumber: Int = 14
            val nodesLimit: Int = 13
            //val expectedNodesInSCC: Int = 3
            val expectedSize: Int =
            //6
              13
            val mockUpOneNode: Vector[Arc] =
              Vector(
                      Arc(1, 1)
                    )
            val mockUpTwoNodes: Vector[Arc] =
              Vector(
                      Arc(1, 2)
                    )
            val mockUpThreeNodes1: Vector[Arc] =
              Vector(
                      Arc(1, 2),
                      Arc(1, 3)
                    )
            val mockUpThreeNodes2: Vector[Arc] =
              Vector(
                      Arc(1, 2),
                      Arc(2, 3)
                    )
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
            val graphNodes: Array[IsExploredNode] =
              (1 to nodesLimit)
              .map(IsExploredNode(_, false))
              .toArray
            //val nodesInSCC: Int =
            val nodesInSCC: Int =
              BFS_SCC_NodesAmountImproved(
                                           graph = mockUpTree2,
                                           startingNode = 1,
                                           graphNodes = graphNodes
                                         )

            println(
                     s"\n'nodesInSCC` is:${nodesInSCC}")
            assume(
                    //true == true,
                    nodesInSCC == expectedSize,
                    s"\nnumber of 'nodesInSCC' must be equal to " +
                      s"'expectedSize'"
                  )
          }
  ignore(
          "21: 'findAllSCCwithBFS'" +
            "should return all connected components in graph"
        ) {
            val takeNumber: Int = 14
            //val expectedNodesInSCC: Int = 3
            val expectedSize: Int =
            //6
              3
            val mockUpOneNode: Vector[Arc] =
              Vector(
                      Arc(1, 1)
                    )
            val mockUpTwoNodes: Vector[Arc] =
              Vector(
                      Arc(1, 2)
                    )
            val mockUpThreeNodes1: Vector[Arc] =
              Vector(
                      Arc(1, 2),
                      Arc(1, 3)
                    )
            val mockUpThreeNodes2: Vector[Arc] =
              Vector(
                      Arc(1, 2),
                      Arc(2, 3)
                    )
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
            val mockUpGraphWith3SCC: Vector[Arc] =
              Vector(
                      Arc(1, 2),
                      Arc(3, 5),
                      Arc(3, 4),
                      Arc(4, 5),
                      Arc(6, 6)
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
              extractNodesFromArcs(mockUpGraphWith3SCC)
            //val nodesInSCC: Int =
            val exploredSCCs: Seq[Int] =
              findAllSCCwithBFS(
                                 graph = mockUpGraphWith3SCC,
                                 unExploredGraphNodes = nodes
                               )

            println(
                     s"\n'exploredSCCs.length` is:${exploredSCCs.length}" +
                       s"\n'nodes` in each SCC:\n${exploredSCCs.mkString("\n")}"
                   )
            assume(
                    //true == true,
                    exploredSCCs.length == expectedSize,
                    s"\nnumber of 'exploredSCCs' must be equal to " +
                      s"'expectedSize'"
                  )
          }
  ignore(
          "22: 'findAllSCCwithBFSImproved'" +
            "should return " +
            "the `sizes` of " +
            "the '5' largest `SCCs` in the given `graph`"
          //"all connected components in graph from big input"
        ) {
            val takeNumber: Int = 5
            val nodesLimit: Int = 875714
            //val expectedNodesInSCC: Int = 3
            //val expectedSize: Int =
            //6
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
            val graphNodes: Array[IsExploredNode] =
              (1 to nodesLimit)
              .map(IsExploredNode(_, false))
              .toArray
            //val nodesInSCC: Int =
            /*too slow on big input*/
            val exploredSCCs: Seq[Int] =
              findAllSCCwithBFSImproved(
                                         graph = arcs,
                                         graphNodes = graphNodes,
                                         nodesLimit = nodesLimit
                                       )

            println(
                     s"\n'exploredSCCs.length` is:${exploredSCCs.length}" +
                       s"\n'nodes` in first $takeNumber SCC:" +
                       s"\n${
                         exploredSCCs
                         .sorted(Ordering[Int].reverse)
                         .take(takeNumber)
                         .mkString("\n")
                       }"
                   )
            assume(
                    //true == true,
                    exploredSCCs.nonEmpty,
                    s"\nnumber of 'exploredSCCs' must be 'nonEmpty'"
                  )
          }
  ignore(
          "31: 'setArcsUnExplored'" +
            "should return " +
            "'arcs' in the special format"
        ) {
            val takeNumber: Int = 5
            val nodesLimit: Int = 875714
            //val expectedNodesInSCC: Int = 3
            //val expectedSize: Int =
            //6
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
            /*val arcs: Vector[Arc] =
              extractArcs(actualFileContent)*/
            val DirectedGraph(nodes, arcs): DirectedGraph =
              extractArcsAndNodes(actualFileContent)

            /*println(
                     s"\n'arcs.head` is:${arcs.head}"
                   )*/
            println(
                     s"\n'arcs.length` is:${arcs.length}" +
                       s"\nfirst $takeNumber 'arcs` are:" +
                       s"\n${
                         arcs
                         .take(takeNumber)
                         .mkString("\n")
                       }"
                   )


            //val unExploredNodes: Array[IsExploredNode] =
            val unExploredNodes: Vector[IsExploredNode] =
              nodes
              .map(IsExploredNode(_, false))
              .toVector
            //.toArray

            println(
                     s"\n'unExploredNodes.head` is:${unExploredNodes.head}"
                   )

            /*?too slow on big input?*/
            val unExploredArcs: Seq[ArcFromNodes] =
              setArcsUnExplored(
                                 nodes = unExploredNodes,
                                 arcsRemain = arcs
                               )

            println(
                     s"\n'unExploredArcs.length` is:${unExploredArcs.length}" +
                       s"\nfirst $takeNumber 'unExploredArcs` are:" +
                       s"\n${
                         unExploredArcs
                         .take(takeNumber)
                         .mkString("\n")
                       }"
                   )
            assume(
                    //true == true,
                    unExploredArcs.nonEmpty &&
                      unExploredArcs.length == arcs.length,
                    s"\n'unExploredArcs' must be 'nonEmpty' & sorted by 'tail'"
                  )
          }
  ignore(
          "32: 'makeAdjacencyListFromArcs'" +
            "should return " +
            "'AdjacencyList'"
        ) {
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
                    ).sortBy(_.arcTail)
            val takeNumber: Int = 5
            val nodesLimit: Int =
              875714
            //13
            //val expectedNodesInSCC: Int = 3
            //val expectedSize: Int =
            //6
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
              extractSortedArcs(actualFileContent)
            //extractArcs(actualFileContent)
            //mockUpTree1
            /*val DirectedGraph(nodes, arcs): DirectedGraph =
              extractArcsAndNodes(actualFileContent)*/

            /*println(
                     s"\n'arcs.head` is:${arcs.head}"
                   )*/
            /*println(
                     s"\n'arcs.length` is:${arcs.length}" +
                       s"\nfirst $takeNumber 'arcs` are:" +
                       s"\n${
                         arcs
                         .take(takeNumber)
                         .mkString("\n")
                       }"
                   )*/

            /*!!!Warn: 'arcs' must be sorted by 'arcTail'!!!*/
            val adjacencyList: Vector[AdjacencyListElem] =
              makeAdjacencyListFromArcs(
                                         minNodeVal = 1,
                                         maxNodeVal =
                                           nodesLimit,
                                         currentNodeVal = 1 - 1,
                                         arcsRemains = arcs.toList
                                       )

            println(
                     s"\n'adjacencyList.length` is:${adjacencyList.length}" +
                       s"\nfirst $takeNumber 'unExploredArcs` are:" +
                       s"\n${
                         adjacencyList
                         .take(takeNumber)
                         .mkString("\n")
                       }"
                   )
            assume(
                    //true == true,
                    adjacencyList.nonEmpty &&
                      adjacencyList.length == nodesLimit,
                    s"\n'adjacencyList' must be 'nonEmpty' & sorted by 'tail'"
                  )
          }
  ignore(
          "33: 'makeIndexedNodeWithAdjacencyListFromArcs'" +
            "should return " +
            "'IndexedNodeWithAdjacencyList'"
        ) {
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
                    ).sortBy(_.arcTail)
            val mockUpSCCwith4PartsArcs: Vector[Arc] =
              Vector(
                      //size = 2
                      Arc(1, 2),
                      Arc(2, 3),
                      Arc(3, 1),
                      //size =2
                      Arc(4, 2),
                      Arc(4, 3),
                      Arc(4, 5),
                      Arc(5, 4),
                      Arc(5, 6),
                      //size = 2
                      Arc(6, 3),
                      Arc(6, 7),
                      Arc(7, 6),
                      //size = 1
                      Arc(8, 5),
                      Arc(8, 7),
                      Arc(8, 8)
                    )
            //.sortBy(_.arcTail)
            val correspondingNodes: Vector[IndexedNode] =
              (1 to 8)
              .map(
                  IndexedNode(
                               _,
                               None,
                               Int.MaxValue,
                               //Double.PositiveInfinity.toInt,
                               false))
              .toVector

            val takeNumber: Int = 5
            val nodesLimit: Int =
              correspondingNodes.length
            //875714
            //13
            //val expectedNodesInSCC: Int = 3
            //val expectedSize: Int =
            //6
            val filePath: String =
              "/media/gluk-alex/" +
                "GDI/Java/Scala/sbt/projects/" +
                "stanfordAlgorithmsDesignAndAnalysis1/" +
                "src/test/scala/" +
                "testSCC/"
            //SCC.txt
            val fileName: String = "SCC.txt"
            val actualFileContent: Iterator[String] =
              Iterator.empty
            /*readFromFile(
                          fileName = fileName,
                          filePath = filePath
                        )*/
            val arcs: Vector[Arc] =
            //extractSortedArcs(actualFileContent)
              mockUpSCCwith4PartsArcs
            /*val DirectedGraph(nodes, arcs): DirectedGraph =
              extractArcsAndNodes(actualFileContent)*/

            /*println(
                     s"\n'arcs.head` is:${arcs.head}"
                   )*/
            /*println(
                     s"\n'arcs.length` is:${arcs.length}" +
                       s"\nfirst $takeNumber 'arcs` are:" +
                       s"\n${
                         arcs
                         .take(takeNumber)
                         .mkString("\n")
                       }"
                   )*/

            /*!!!Warn: 'arcs' must be sorted by 'arcTail'!!!*/
            val indexedAdjacencyList: Vector[IndexedNodeWithAdjacencyList] =
              makeIndexedNodeWithAdjacencyListFromArcs(
                                                        nodes =
                                                          correspondingNodes,
                                                        nodesRemains =
                                                          correspondingNodes
                                                          .toList,
                                                        currentNodeVal = 1 - 1,
                                                        arcsRemains = arcs
                                                                      .toList
                                                      )

            println(
                     s"\n'adjacencyList.length` is:${
                       indexedAdjacencyList.length
                     }" +
                       s"\nfirst $takeNumber 'unExploredArcs` are:" +
                       s"\n${
                         indexedAdjacencyList
                         .take(takeNumber)
                         .mkString("\n")
                       }"
                   )
            assume(
                    //true == true,
                    indexedAdjacencyList.nonEmpty &&
                      indexedAdjacencyList.length == nodesLimit,
                    s"\n'adjacencyList' must be 'nonEmpty' & sorted by 'tail'"
                  )
          }
  /*TODO debug*/
  ignore(
          "34: 'tarjan'" +
            "should return " +
            "'SCC' that it found"
        ) {
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
                    ).sortBy(_.arcTail)
            val mockUpSCCwith4PartsArcs: Vector[Arc] =
              Vector(
                      //size = 2
                      Arc(1, 2),
                      Arc(2, 3),
                      Arc(3, 1),
                      //size =2
                      Arc(4, 2),
                      Arc(4, 3),
                      Arc(4, 5),
                      Arc(5, 4),
                      Arc(5, 6),
                      //size = 2
                      Arc(6, 3),
                      Arc(6, 7),
                      Arc(7, 6),
                      //size = 1
                      Arc(8, 5),
                      Arc(8, 7),
                      Arc(8, 8)
                    )
            //.sortBy(_.arcTail)
            val correspondingNodes: Vector[IndexedNode] =
              (1 to 8)
              .map(
                  IndexedNode(
                               _,
                               None,
                               Int.MaxValue,
                               //Double.PositiveInfinity.toInt,
                               false))
              .toVector

            val takeNumber: Int = 5
            val nodesLimit: Int =
              correspondingNodes.length
            //875714
            //13
            //val expectedNodesInSCC: Int = 3
            val expectedSize: Int =
              4
            val filePath: String =
              "/media/gluk-alex/" +
                "GDI/Java/Scala/sbt/projects/" +
                "stanfordAlgorithmsDesignAndAnalysis1/" +
                "src/test/scala/" +
                "testSCC/"
            //SCC.txt
            val fileName: String = "SCC.txt"
            val actualFileContent: Iterator[String] =
              Iterator.empty
            /*readFromFile(
                          fileName = fileName,
                          filePath = filePath
                        )*/
            val arcs: Vector[Arc] =
            //extractSortedArcs(actualFileContent)
              mockUpSCCwith4PartsArcs
            /*val DirectedGraph(nodes, arcs): DirectedGraph =
              extractArcsAndNodes(actualFileContent)*/

            /*println(
                     s"\n'arcs.head` is:${arcs.head}"
                   )*/
            /*println(
                     s"\n'arcs.length` is:${arcs.length}" +
                       s"\nfirst $takeNumber 'arcs` are:" +
                       s"\n${
                         arcs
                         .take(takeNumber)
                         .mkString("\n")
                       }"
                   )*/

            /*!!!Warn: 'arcs' must be sorted by 'arcTail'!!!*/
            val indexedAdjacencyList: Vector[IndexedNodeWithAdjacencyList] =
              makeIndexedNodeWithAdjacencyListFromArcs(
                                                        nodes =
                                                          correspondingNodes,
                                                        nodesRemains =
                                                          correspondingNodes
                                                          .toList,
                                                        currentNodeVal = 1 - 1,
                                                        arcsRemains = arcs
                                                                      .toList
                                                      )
            val graphSCCs =
              tarjan(indexedAdjacencyList)

            println(
                     s"\n'graphSCCs.length` is:${
                       graphSCCs.length
                     }" +
                       s"\nfirst $takeNumber 'unExploredArcs` are:" +
                       s"\n${
                         graphSCCs
                         .take(takeNumber)
                         .mkString("\n")
                       }"
                   )
            assume(
                    //true == true,
                    graphSCCs.nonEmpty &&
                      indexedAdjacencyList.length == expectedSize,
                    s"\n'graphSCCs' must be 'nonEmpty' " +
                      s"& equal to 'expectedSize'"
                  )
          }
  ignore(
          "35: 'Depth-first search (DFS)' with `preOrder`" +
            "should return " +
            "all reachable 'nodes' from the specific 'node'"
        ) {
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
                    ).sortBy(_.arcTail)
            val mockUpSCCwith4PartsArcs: Vector[Arc] =
              Vector(
                      //size = 2
                      Arc(1, 2),
                      Arc(2, 3),
                      Arc(3, 1),
                      //size =2
                      Arc(4, 2),
                      Arc(4, 3),
                      Arc(4, 5),
                      Arc(5, 4),
                      Arc(5, 6),
                      //size = 2
                      Arc(6, 3),
                      Arc(6, 7),
                      Arc(7, 6),
                      //size = 1
                      Arc(8, 5),
                      Arc(8, 7),
                      Arc(8, 8)
                    )
            val tinyDG: Vector[Arc] =
              Vector(
                      Arc(4, 2),
                      Arc(2, 3),
                      Arc(3, 2),
                      Arc(6, 0),
                      Arc(0, 1),
                      Arc(2, 0),
                      Arc(11, 12),
                      Arc(12, 9),
                      Arc(9, 10),
                      Arc(9, 11),
                      Arc(7, 9),
                      Arc(10, 12),
                      Arc(11, 4),
                      Arc(4, 3),
                      Arc(3, 5),
                      Arc(6, 8),
                      Arc(8, 6),
                      Arc(5, 4),
                      Arc(0, 5),
                      Arc(6, 4),
                      Arc(6, 9),
                      Arc(7, 6)
                    )
            val tinyDAG: Vector[Arc] =
              Vector(
                      Arc(2, 3),
                      Arc(0, 6),
                      Arc(0, 1),
                      Arc(2, 0),
                      Arc(11, 12),
                      Arc(9, 12),
                      Arc(9, 10),
                      Arc(9, 11),
                      Arc(3, 5),
                      Arc(8, 7),
                      Arc(5, 4),
                      Arc(0, 5),
                      Arc(6, 4),
                      Arc(6, 9),
                      Arc(7, 6)
                    )
              .map(a => Arc(a.arcTail + 1, a.arcHead + 1))
              .sortBy(_.arcTail)
            val nodesInGraph: Int = 13
            val minNodeVal: Int =
            //0
              1
            val maxNodeVal: Int =
            //12
              13
            //8
            //val correspondingNodes: Vector[IndexedNode] =
            val correspondingNodes: Vector[IsExploredNode] =
              (minNodeVal to maxNodeVal)
              .map(
                  /*IndexedNode(
                               _,
                               None,
                               Int.MaxValue,
                               //Double.PositiveInfinity.toInt,
                               false))*/
                  IsExploredNode(_, false)
                  )
              .toVector

            val startingNode: Int =
            //0
            //1
              9
            //8
            val takeNumber: Int = 15
            val nodesLimit: Int =
              correspondingNodes.length
            //875714
            //13
            //val expectedNodesInSCC: Int = 3
            val expectedSize: Int =
            //3
              correspondingNodes.length
            val filePath: String =
              "/media/gluk-alex/" +
                "GDI/Java/Scala/sbt/projects/" +
                "stanfordAlgorithmsDesignAndAnalysis1/" +
                "src/test/scala/" +
                "testSCC/"
            //SCC.txt
            val fileName: String = "SCC.txt"
            val actualFileContent: Iterator[String] =
              Iterator.empty
            /*readFromFile(
                          fileName = fileName,
                          filePath = filePath
                        )*/
            val arcs: Vector[Arc] =
              tinyDAG
            //mockUpSCCwith4PartsArcs
            //extractSortedArcs(actualFileContent)
            /*val DirectedGraph(nodes, arcs): DirectedGraph =
              extractArcsAndNodes(actualFileContent)*/

            /*println(
                     s"\n'arcs.head` is:${arcs.head}"
                   )*/
            /*println(
                     s"\n'arcs.length` is:${arcs.length}" +
                       s"\nfirst $takeNumber 'arcs` are:" +
                       s"\n${
                         arcs
                         .take(takeNumber)
                         .mkString("\n")
                       }"
                   )*/

            /*!!!Warn: 'arcs' must be sorted by 'arcTail'!!!*/
            val explorableAdjacencyList: Vector[ExplorableNodeWithAdjusted] =
              makeExplorableAdjacencyListFromArcs(
                                                   nodes =
                                                     correspondingNodes
                                                     .toVector,
                                                   /*nodesRemains =
                                                     correspondingNodes
                                                     //.toList,
                                                     .toStream,*/
                                                   currentNodeVal =
                                                     if (minNodeVal == 0) {
                                                       minNodeVal
                                                     } else {
                                                       minNodeVal - 1
                                                     },
                                                   arcsRemains =
                                                     arcs
                                                     //.toList
                                                     .toStream
                                                 )
            //val reachableNodes: List[IsExploredNode] =
            val reachableNodes: Stream[IsExploredNode] =
            //8[e],5[e],4[e],2[e],3[e],1[e],6[e],7[e]
              preOrderDFS(
                           //1[e],3[e],2[e],4[e],7[e],6[e],5[e],8[e]
                           //postOrderDFS(
                           explorableAdjacencyList,
                           v = if (startingNode == 0) {
                             1
                           } else {
                             startingNode
                           }
                         )

            println(
                     s"\n'nodes` reachable from :$startingNode" +
                       s"\n'reachableNodes.length` is:${
                         reachableNodes.length
                       }" +
                       s"\nfirst $takeNumber 'reachableNodes` are:" +
                       s"\n${
                         reachableNodes
                         .take(takeNumber)
                         .mkString(",")
                       }"
                   )
            println(
                     s"\n'explorableAdjacencyList` became:" +
                       s"\nfirst $takeNumber in 'explorableAdjacencyList` " +
                       s"are:" +
                       s"\n${
                         explorableAdjacencyList
                         .take(takeNumber)
                         .mkString("\n")
                       }"
                   )
            assume(
                    //true == true,
                    reachableNodes.nonEmpty &&
                      reachableNodes.length == expectedSize,
                    s"\n'reachableNodes' must be 'nonEmpty' " +
                      s"& equal to 'expectedSize'"
                  )
          }
  /*TODO bug on file input*/
  ignore(
          "36: 'DepthFirstOrder' " +
            "should return " +
            "a depth-first order for the diGraph"
        ) {
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
                    ).sortBy(_.arcTail)
            val mockUpSCCwith4PartsArcs: Vector[Arc] =
              Vector(
                      //size = 2
                      Arc(1, 2),
                      Arc(2, 3),
                      Arc(3, 1),
                      //size =2
                      Arc(4, 2),
                      Arc(4, 3),
                      Arc(4, 5),
                      Arc(5, 4),
                      Arc(5, 6),
                      //size = 2
                      Arc(6, 3),
                      Arc(6, 7),
                      Arc(7, 6),
                      //size = 1
                      Arc(8, 5),
                      Arc(8, 7),
                      Arc(8, 8)
                    )
            val transposeSCCwith4PartsArcs: Vector[Arc] =
              mockUpSCCwith4PartsArcs
              .map(a => Arc(a.arcHead, a.arcTail))
              .sortBy(_.arcTail)
            val tinyDG: Vector[Arc] =
              Vector(
                      Arc(4, 2),
                      Arc(2, 3),
                      Arc(3, 2),
                      Arc(6, 0),
                      Arc(0, 1),
                      Arc(2, 0),
                      Arc(11, 12),
                      Arc(12, 9),
                      Arc(9, 10),
                      Arc(9, 11),
                      Arc(7, 9),
                      Arc(10, 12),
                      Arc(11, 4),
                      Arc(4, 3),
                      Arc(3, 5),
                      Arc(6, 8),
                      Arc(8, 6),
                      Arc(5, 4),
                      Arc(0, 5),
                      Arc(6, 4),
                      Arc(6, 9),
                      Arc(7, 6)
                    )
            val transposeTinyDG: Vector[Arc] =
              tinyDG
              .map(a => Arc(a.arcHead, a.arcTail))
              .sortBy(_.arcTail)
            val tinyDAG: Vector[Arc] =
              Vector(
                      Arc(2, 3),
                      Arc(0, 6),
                      Arc(0, 1),
                      Arc(2, 0),
                      Arc(11, 12),
                      Arc(9, 12),
                      Arc(9, 10),
                      Arc(9, 11),
                      Arc(3, 5),
                      Arc(8, 7),
                      Arc(5, 4),
                      Arc(0, 5),
                      Arc(6, 4),
                      Arc(6, 9),
                      Arc(7, 6)
                    )
              .map(a => Arc(a.arcTail + 1, a.arcHead + 1))
              .sortBy(_.arcTail)
            val transposeTinyDAG: Vector[Arc] =
              tinyDG
              .map(a => Arc(a.arcHead, a.arcTail))
              .sortBy(_.arcTail)
            val nodesInGraph: Int = 13
            val minNodeVal: Int =
            //0
              1
            val maxNodeVal: Int =
            //12
              13
            //8
            //val correspondingNodes: Vector[IndexedNode] =
            val correspondingNodes: Vector[IsExploredNode] =
              (minNodeVal to maxNodeVal)
              .map(
                  /*IndexedNode(
                               _,
                               None,
                               Int.MaxValue,
                               //Double.PositiveInfinity.toInt,
                               false))*/
                  IsExploredNode(_, false)
                  )
              .toVector

            val startingNode: Int =
            //0
            //1
              9
            //8
            val takeNumber: Int = 15
            val nodesLimit: Int =
              correspondingNodes.length
            //875714
            //13
            //val expectedNodesInSCC: Int = 3
            val expectedSize: Int =
            //3
              correspondingNodes.length
            val filePath: String =
              "/media/gluk-alex/" +
                "GDI/Java/Scala/sbt/projects/" +
                "stanfordAlgorithmsDesignAndAnalysis1/" +
                "src/test/scala/" +
                "testSCC/"
            //SCC.txt
            val fileName: String = "SCC.txt"
            val actualFileContent: Iterator[String] =
            //Iterator.empty
              readFromFile(
                            fileName = fileName,
                            filePath = filePath
                          )
            val arcs: Vector[Arc] =
            //tinyDAG
            //transposeTinyDAG
            //mockUpSCCwith4PartsArcs
              extractSortedArcs(actualFileContent)
            /*val DirectedGraph(nodes, arcs): DirectedGraph =
              extractArcsAndNodes(actualFileContent)*/

            /*println(
                     s"\n'arcs.head` is:${arcs.head}"
                   )*/
            /*println(
                     s"\n'arcs.length` is:${arcs.length}" +
                       s"\nfirst $takeNumber 'arcs` are:" +
                       s"\n${
                         arcs
                         .take(takeNumber)
                         .mkString("\n")
                       }"
                   )*/

            /*!!!Warn: 'arcs' must be sorted by 'arcTail'!!!*/
            val explorableAdjacencyList: Vector[ExplorableNodeWithAdjusted] =
              makeExplorableAdjacencyListFromArcs(
                                                   nodes =
                                                     correspondingNodes
                                                     .toVector,
                                                   /*nodesRemains =
                                                     correspondingNodes
                                                     .toStream,*/
                                                   //.toList,
                                                   currentNodeVal =
                                                     if (minNodeVal == 0) {
                                                       minNodeVal
                                                     } else {
                                                       minNodeVal - 1
                                                     },
                                                   arcsRemains =
                                                     arcs
                                                     .toStream
                                                   //.toList
                                                 )
            //val depthFirstOrder: DepthFirstSearchResult =
            val DepthFirstSearchResult(
            preOrd,
            postOrd): DepthFirstSearchResult =
              DepthFirstOrder(
                               graph = explorableAdjacencyList,
                               graphLength = explorableAdjacencyList.length,
                               nodesValuesZeroBased =
                                 minNodeVal == 0
                             )
            //first 15 'nodes' in 'preOrd` are:
            //1[e],7[e],5[e],10[e],13[e],11[e],12[e],2[e],6[e],3[e],4[e],
            // 8[e],9[e]
            //5[e],13[e],11[e],12[e],10[e],7[e],2[e],6[e],1[e],4[e],3[e],
            // 8[e],9[e]
            //first 15 'nodes' in 'postOrd` are:
            println(
                     s"\n'preOrd.length` is:${
                       preOrd.length
                     }" +
                       s"\nfirst $takeNumber 'nodes' in 'preOrd` are:" +
                       s"\n${
                         preOrd
                         .take(takeNumber)
                         .mkString(",")
                       }"
                   )
            println(
                     s"\n'postOrd.length` is:${
                       postOrd.length
                     }" +
                       s"\nfirst $takeNumber 'nodes' in 'postOrd` are:" +
                       s"\n${
                         postOrd
                         .take(takeNumber)
                         .mkString(",")
                       }"
                   )
            println(
                     s"\n'explorableAdjacencyList` became:" +
                       s"\nfirst $takeNumber in 'explorableAdjacencyList` " +
                       s"are:" +
                       s"\n${
                         explorableAdjacencyList
                         .take(takeNumber)
                         .mkString("\n")
                       }"
                   )
            assume(
                    //true == true,
                    postOrd.nonEmpty &&
                      postOrd.length == expectedSize,
                    s"\n'postOrd' must be 'nonEmpty' " +
                      s"& equal to 'expectedSize'"
                  )
          }
  ignore(
          "37: 'DepthFirstOrder' 'preOrderDFS' then `transpose` `graph` " +
            "should return " +
            "result reset as `unExplored`"
        ) {
            val mockUpSCCwith4PartsArcs: Vector[Arc] =
              Vector(
                      //size = 2
                      Arc(1, 2),
                      Arc(2, 3),
                      Arc(3, 1),
                      //size =2
                      Arc(4, 2),
                      Arc(4, 3),
                      Arc(4, 5),
                      Arc(5, 4),
                      Arc(5, 6),
                      //size = 2
                      Arc(6, 3),
                      Arc(6, 7),
                      Arc(7, 6),
                      //size = 1
                      Arc(8, 5),
                      Arc(8, 7),
                      Arc(8, 8)
                    )
            val transposeSCCwith4PartsArcs: Vector[Arc] =
              mockUpSCCwith4PartsArcs
              .map(a => Arc(a.arcHead, a.arcTail))
              .sortBy(_.arcTail)
            val nodesInGraph: Int = 8
            val minNodeVal: Int = 1
            val maxNodeVal: Int = nodesInGraph
            val correspondingNodes: Vector[IsExploredNode] =
              (minNodeVal to maxNodeVal)
              .map(
                  IsExploredNode(_, false)
                  )
              .toVector
            val startingNode: Int =
            //0
            //1
              9
            //8
            val takeNumber: Int = 15
            val nodesLimit: Int =
              correspondingNodes.length
            //875714
            //13
            //val expectedNodesInSCC: Int = 3
            val expectedSize: Int =
            //3
              correspondingNodes.length
            val filePath: String =
              "/media/gluk-alex/" +
                "GDI/Java/Scala/sbt/projects/" +
                "stanfordAlgorithmsDesignAndAnalysis1/" +
                "src/test/scala/" +
                "testSCC/"
            //SCC.txt
            val fileName: String = "SCC.txt"
            val actualFileContent: Iterator[String] =
              Iterator.empty
            /*readFromFile(
                          fileName = fileName,
                          filePath = filePath
                        )*/
            val arcs: Vector[Arc] =
              mockUpSCCwith4PartsArcs
            //extractSortedArcs(actualFileContent)
            /*val DirectedGraph(nodes, arcs): DirectedGraph =
              extractArcsAndNodes(actualFileContent)*/

            /*println(
                     s"\n'arcs.head` is:${arcs.head}"
                   )*/
            /*println(
                     s"\n'arcs.length` is:${arcs.length}" +
                       s"\nfirst $takeNumber 'arcs` are:" +
                       s"\n${
                         arcs
                         .take(takeNumber)
                         .mkString("\n")
                       }"
                   )*/

            /*!!!Warn: 'arcs' must be sorted by 'arcTail'!!!*/
            val explorableAdjacencyList: Vector[ExplorableNodeWithAdjusted] =
              makeExplorableAdjacencyListFromArcs(
                                                   nodes =
                                                     correspondingNodes,
                                                   /*nodesRemains =
                                                     correspondingNodes
                                                     .toStream,*/
                                                   //.toList,
                                                   currentNodeVal =
                                                     minNodeVal - 1,
                                                   arcsRemains =
                                                     arcs
                                                     .toStream,
                                                   //.toList,
                                                   minNodeVal = minNodeVal,
                                                   nodeIndexShift = -1
                                                 )
            println(
                     s"\n'explorableAdjacencyList` is:" +
                       s"\nfirst $takeNumber in 'transposeAdjacencyList` are:" +
                       s"\n${
                         explorableAdjacencyList
                         .take(takeNumber)
                         .mkString("\n")
                       }"
                   )

            //val depthFirstOrder: DepthFirstSearchResult =
            val DepthFirstSearchResult(
            preOrd,
            postOrd): DepthFirstSearchResult =
              DepthFirstOrder(
                               graph = explorableAdjacencyList,
                               graphLength = explorableAdjacencyList.length,
                               nodesValuesZeroBased =
                                 minNodeVal == 0
                             )
            //first 15 'nodes' in 'preOrd` are:
            //1[e],7[e],5[e],10[e],13[e],11[e],12[e],2[e],6[e],3[e],4[e],
            // 8[e],9[e]
            //5[e],13[e],11[e],12[e],10[e],7[e],2[e],6[e],1[e],4[e],3[e],
            // 8[e],9[e]
            //first 15 'nodes' in 'postOrd` are:
            println(
                     s"\n'preOrd.length` is:${
                       preOrd.length
                     }" +
                       s"\nfirst $takeNumber 'nodes' in 'preOrd` are:" +
                       s"\n${
                         preOrd
                         .take(takeNumber)
                         .mkString(",")
                       }"
                   )
            /*println(
                     s"\n'postOrd.length` is:${
                       postOrd.length
                     }" +
                       s"\nfirst $takeNumber 'nodes' in 'postOrd` are:" +
                       s"\n${
                         postOrd
                         .take(takeNumber)
                         .mkString(",")
                       }"
                   )*/
            /*reset 'nodes' as `unExplored`*/
            correspondingNodes
            .map(_.isExplored = false)
            /*!!!Warn: 'arcs' must be sorted by 'arcTail'!!!*/
            val transposeAdjacencyList: Vector[ExplorableNodeWithAdjusted] =
              makeExplorableAdjacencyListFromArcs(
                                                   nodes =
                                                     correspondingNodes,
                                                   /*nodesRemains =
                                                     correspondingNodes
                                                     .toStream,*/
                                                   //.toList,
                                                   currentNodeVal =
                                                     minNodeVal - 1,
                                                   arcsRemains =
                                                     transposeSCCwith4PartsArcs
                                                     .toStream,
                                                   //.toList,
                                                   minNodeVal = minNodeVal,
                                                   nodeIndexShift = -1
                                                 )

            println(
                     s"\n'transposeAdjacencyList` became:" +
                       s"\nfirst $takeNumber in 'transposeAdjacencyList` are:" +
                       s"\n${
                         transposeAdjacencyList
                         .take(takeNumber)
                         .mkString("\n")
                       }"
                   )
            assume(
                    //true == true,
                    postOrd.nonEmpty &&
                      postOrd.length == expectedSize,
                    s"\n'postOrd' must be 'nonEmpty' " +
                      s"& equal to 'expectedSize'"
                  )
          }
  ignore(
          "38: 'transposeDepthFirstOrderSCCs' " +
            "should return " +
            "all SCCs that it find " +
            "from `transpose` `graph`" +
            "when search in 'preOrderDFS'"
        ) {
            val mockUpSCCwith4PartsArcs: Vector[Arc] =
              Vector(
                      //size = 3
                      Arc(1, 2),
                      Arc(2, 3),
                      Arc(3, 1),
                      //size = 2
                      Arc(4, 2),
                      Arc(4, 3),
                      Arc(4, 5),
                      Arc(5, 4),
                      Arc(5, 6),
                      //size = 2
                      Arc(6, 3),
                      Arc(6, 7),
                      Arc(7, 6),
                      //size = 1
                      Arc(8, 5),
                      Arc(8, 7),
                      Arc(8, 8)
                    )
            val transposeSCCwith4PartsArcs: Vector[Arc] =
              mockUpSCCwith4PartsArcs
              .map(a => Arc(a.arcHead, a.arcTail))
              .sortBy(_.arcTail)
            val nodesInGraph: Int = 8
            val sourceSize: Int = 5105043
            val expectedNodesSize: Int = 875714
            val expectedArcsSize: Int = 5105043
            /*all 'nodes', but only few / some 'arcs'*/
            val inputTakeNumber: Int = 100
            val expectedNodesInSCC: Int = 3
            val expectedSize: Int = 4
            val filePath: String =
              "/media/gluk-alex/" +
                "GDI/Java/Scala/sbt/projects/" +
                "stanfordAlgorithmsDesignAndAnalysis1/" +
                "src/test/scala/" +
                "testSCC/"
            //SCC.txt
            val fileName: String = "SCC.txt"
            val actualFileContent: Iterator[String] =
            //Iterator.empty
              readFromFile(
                            fileName = fileName,
                            filePath = filePath
                          )
            val arcs: Vector[Arc] =
            //transposeSCCwith4PartsArcs
            //mockUpSCCwith4PartsArcs
              extractSortedArcs(actualFileContent)
            val transposedArcs: Vector[Arc] =
              arcs
              .map(a => Arc(a.arcHead, a.arcTail))
              .sortBy(_.arcTail)
            val minNodeVal: Int = 1
            val maxNodeVal: Int =
            //expectedNodesSize
              arcs
              .maxBy(_.arcHead)
              .arcHead
              .max(arcs
                   .maxBy(_.arcTail)
                   .arcHead)
            println(s"\n'maxNodeVal' is $maxNodeVal")
            val correspondingNodes: Vector[IsExploredNode] =
              (minNodeVal to maxNodeVal)
              .map(
                  IsExploredNode(_, false)
                  )
              .toVector
            val startingNode: Int = 1
            val takeNumber: Int = 15
            val nodesLimit: Int =
            //correspondingNodes.length
              maxNodeVal
            /*val DirectedGraph(nodes, arcs): DirectedGraph =
              extractArcsAndNodes(actualFileContent)*/

            /*println(
                     s"\n'arcs.head` is:${arcs.head}"
                   )*/
            /*println(
                     s"\n'arcs.length` is:${arcs.length}" +
                       s"\nfirst $takeNumber 'arcs` are:" +
                       s"\n${
                         arcs
                         .take(takeNumber)
                         .mkString("\n")
                       }"
                   )*/

            /*!!!Warn: 'arcs' must be sorted by 'arcTail'!!!*/
            val explorableAdjacencyList: Vector[ExplorableNodeWithAdjusted] =
              makeExplorableAdjacencyListFromArcs(
                                                   nodes =
                                                     correspondingNodes,
                                                   /*nodesRemains =
                                                     correspondingNodes
                                                     .toStream,*/
                                                   //.toList,
                                                   currentNodeVal =
                                                     minNodeVal - 1,
                                                   arcsRemains =
                                                     //arcs
                                                     transposedArcs
                                                     .view
                                                     .take(inputTakeNumber)
                                                     .toStream,
                                                   //.toList,
                                                   minNodeVal = minNodeVal,
                                                   nodeIndexShift = -1
                                                 )
            println(
                     s"\n'explorableAdjacencyList.head' is ${
                       explorableAdjacencyList.head
                     }")
            /*println(
                     s"\n'explorableAdjacencyList` is:" +
                       s"\nfirst $takeNumber in 'transposeAdjacencyList` are:" +
                       s"\n${
                         explorableAdjacencyList
                         .take(takeNumber)
                         .mkString("\n")
                       }"
                   )*/

            //val depthFirstOrder: DepthFirstSearchResult =
            /*only post order needed*/
            /*val DepthFirstSearchResult(
            preOrd,
            postOrd): DepthFirstSearchResult =
              DepthFirstOrder(
                               graph = explorableAdjacencyList,
                               graphLength =
                                 explorableAdjacencyList
                                 .length,
                               nodesValuesZeroBased =
                                 minNodeVal == 0
                             )*/

            //val postOrd: List[IsExploredNode] =
            val postOrd: Stream[IsExploredNode] =
              DepthFirstPostOrder(
                                   graph = explorableAdjacencyList,
                                   graphLength =
                                     explorableAdjacencyList
                                     .length
                                 )
            //first 15 'nodes' in 'preOrd` are:
            //1[e],7[e],5[e],10[e],13[e],11[e],12[e],2[e],6[e],3[e],4[e],
            // 8[e],9[e]
            //5[e],13[e],11[e],12[e],10[e],7[e],2[e],6[e],1[e],4[e],3[e],
            // 8[e],9[e]
            //first 15 'nodes' in 'postOrd` are:
            /*println(
                     s"\n'preOrd.length` is:${
                       preOrd.length
                     }" +
                       s"\nfirst $takeNumber 'nodes' in 'preOrd` are:" +
                       s"\n${
                         preOrd
                         .take(takeNumber)
                         .mkString(",")
                       }"
                   )*/
            /*println(
                     s"\n'postOrd.length` is:${
                       postOrd.length
                     }" +
                       s"\nfirst $takeNumber 'nodes' in 'postOrd` are:" +
                       s"\n${
                         postOrd
                         .take(takeNumber)
                         .mkString(",")
                       }"
                   )*/
            /*reset 'nodes' as `unExplored`*/
            correspondingNodes
            .map(_.isExplored = false)
            /*!!!Warn: 'arcs' must be sorted by 'arcTail'!!!*/
            val transposeAdjacencyList: Vector[ExplorableNodeWithAdjusted] =
              makeExplorableAdjacencyListFromArcs(
                                                   nodes =
                                                     correspondingNodes,
                                                   /*nodesRemains =
                                                     correspondingNodes
                                                     .toStream,*/
                                                   //.toList,
                                                   currentNodeVal =
                                                     minNodeVal - 1,
                                                   arcsRemains =
                                                     arcs
                                                     .view
                                                     //*mockUpSCCwith4PartsArcs
                                                     //transposeSCCwith4PartsArcs
                                                     .take(inputTakeNumber)
                                                     .toStream,
                                                   //.toList,
                                                   minNodeVal = minNodeVal,
                                                   nodeIndexShift = -1
                                                 )

            println(
                     s"\n'transposeAdjacencyList` became:" +
                       s"\nfirst $takeNumber in 'transposeAdjacencyList` are:" +
                       s"\n${
                         transposeAdjacencyList
                         .take(takeNumber)
                         .mkString("\n")
                       }"
                   )
            /*Reverse `postOrder` needed*/
            //val graphSCCs: List[List[IsExploredNode]] =
            val graphSCCs: Stream[Stream[IsExploredNode]] =
              transposeDepthFirstOrderSCCs(
                                            graph =
                                              transposeAdjacencyList,
                                            preOrderRemains =
                                              postOrd
                                              //preOrd
                                              .reverse,
                                            graphLength =
                                              transposeAdjacencyList.length
                                          )
            println(
                     s"\n'graphSCCs.length` is:${graphSCCs.length}" +
                       s"\nfirst $takeNumber SCCs in 'graphSCCs` are:" +
                       s"\n${
                         graphSCCs
                         .take(takeNumber)
                         .mkString("\n")
                       }"
                   )

            assume(
                    //true == true,
                    graphSCCs.nonEmpty &&
                      graphSCCs.length == expectedSize,
                    s"\n'graphSCCs' must be 'nonEmpty' " +
                      s"& equal to 'expectedSize'"
                  )
          }
  ignore(
          "39: 'transposeDepthFirstOrderSCCsSize' " +
            "should return " +
            "sizes of all SCCs that it find " +
            "from `transpose` `graph`" +
            "when search in 'preOrderDFS'"
        ) {
            val mockUpSCCwith4PartsArcs: Vector[Arc] =
              Vector(
                      //size = 3
                      Arc(1, 2),
                      Arc(2, 3),
                      Arc(3, 1),
                      //size = 2
                      Arc(4, 2),
                      Arc(4, 3),
                      Arc(4, 5),
                      Arc(5, 4),
                      Arc(5, 6),
                      //size = 2
                      Arc(6, 3),
                      Arc(6, 7),
                      Arc(7, 6),
                      //size = 1
                      Arc(8, 5),
                      Arc(8, 7),
                      Arc(8, 8)
                    )
            val transposeSCCwith4PartsArcs: Vector[Arc] =
              mockUpSCCwith4PartsArcs
              .view
              .map(a => Arc(a.arcHead, a.arcTail))
              .sortBy(_.arcTail)
              .toVector
            //seq.view.map(f).flatMap(g).filter(p).toList
            val nodesInGraph: Int = 8
            val sourceSize: Int = 5105043
            val expectedNodesSize: Int = 875714
            val expectedArcsSize: Int = 5105043
            /*all 'nodes', but only few / some 'arcs'*/
            /*at least as big as `mockUp(14)`*/
            val inputTakeNumber: Int = 20
            val expectedNodesInSCC: Int = 3
            val expectedSize: Int = 4
            val filePath: String =
              "/media/gluk-alex/" +
                "GDI/Java/Scala/sbt/projects/" +
                "stanfordAlgorithmsDesignAndAnalysis1/" +
                "src/test/scala/" +
                "testSCC/"
            //SCC.txt
            val fileName: String = "SCC.txt"
            val actualFileContent: Iterator[String] =
            //Iterator.empty
              readFromFile(
                            fileName = fileName,
                            filePath = filePath
                          )
            val arcs: Vector[Arc] =
            //transposeSCCwith4PartsArcs
            //*mockUpSCCwith4PartsArcs //*
              extractSortedArcs(actualFileContent)
              .view
              /*reduce / control input size*/
              .take(inputTakeNumber)
              .toVector

            println(
                     s"\n'arcs` are extracted from file" /*+
                   s"\n'arcs.head` is:${arcs.head}" +
                     s"\n'arcs.tail.head' is: ${
                       arcs.tail.head
                     }"*/
                   )

            /*reversed directions*/
            val transposedArcs: Vector[Arc] =
              arcs
              .view
              .map(a => Arc(a.arcHead, a.arcTail))
              .sortBy(_.arcTail)
              .toVector

            println(
                     s"\n'transposedArcs` evaluated" /*+
                   s"\n'transposedArcs.head` is:${transposedArcs.head}" +
                     s"\n'transposedArcs.tail.head' is: ${
                       transposedArcs.tail.head
                     }"*/
                   )

            val minNodeVal: Int = 1
            val maxNodeVal: Int =
            //expectedNodesSize
              arcs
              .view
              .maxBy(_.arcHead)
              .arcHead
              .max(
                  arcs
                  .view
                  .maxBy(_.arcTail)
                  .arcHead)

            println(s"\n'maxNodeVal' evaluated")
            //println(s"\n'maxNodeVal' is $maxNodeVal")

            val correspondingNodes: Vector[IsExploredNode] =
            /*create new collection as `unexplored`*/
            //def correspondingNodes: IndexedSeq[IsExploredNode] =
              (minNodeVal to maxNodeVal)
              .view
              .map(
                  IsExploredNode(_, false)
                  )
              .toVector
            val startingNode: Int = 1
            val takeNumber: Int = 15
            val nodesLimit: Int =
            //correspondingNodes.length
              maxNodeVal
            /*val DirectedGraph(nodes, arcs): DirectedGraph =
              extractArcsAndNodes(actualFileContent)*/

            /*println(
                     s"\n'arcs.length` is:${arcs.length}" +
                       s"\nfirst $takeNumber 'arcs` are:" +
                       s"\n${
                         arcs
                         .take(takeNumber)
                         .mkString("\n")
                       }"
                   )*/

            /*!!!Warn: 'arcs' must be sorted by 'arcTail'!!!*/
            val explorableAdjacencyList: Vector[ExplorableNodeWithAdjusted] =
              makeExplorableAdjacencyListFromArcs(
                                                   nodes =
                                                     correspondingNodes
                                                     .toVector,
                                                   /*nodesRemains =
                                                     correspondingNodes
                                                     //.toList,
                                                     .toStream,*/
                                                   currentNodeVal =
                                                     minNodeVal - 1,
                                                   arcsRemains =
                                                     arcs
                                                     //*transposedArcs
                                                     //.take(inputTakeNumber)
                                                     //.toList,
                                                     .toStream,
                                                   minNodeVal = minNodeVal,
                                                   maxNodeVal = maxNodeVal,
                                                   //nodesAmount = maxNodeVal,
                                                   nodeIndexShift = -1
                                                 )
            println(
                     s"\n'explorableAdjacencyList.head' is:" //+
                     //explorableAdjacencyList.head
                     /*+
                       s"\n'explorableAdjacencyList.tail.head' is ${
                         explorableAdjacencyList.tail.head
                       }"*/
                   )
            /*println(
                     s"\n'explorableAdjacencyList` is:" +
                       s"\nfirst $takeNumber in 'transposeAdjacencyList` are:" +
                       s"\n${
                         explorableAdjacencyList
                         .take(takeNumber)
                         .mkString("\n")
                       }"
                   )*/

            //val depthFirstOrder: DepthFirstSearchResult =
            /*only post order needed*/
            /*val DepthFirstSearchResult(
            preOrd,
            postOrd): DepthFirstSearchResult =
              DepthFirstOrder(
                               graph = explorableAdjacencyList,
                               graphLength =
                                 explorableAdjacencyList
                                 .length,
                               nodesValuesZeroBased =
                                 minNodeVal == 0
                             )*/

            //val postOrd: List[IsExploredNode] =
            //val postOrd: Stream[IsExploredNode] =
            /*used / evaluated once*/
            //def postOrdReversed: Stream[IsExploredNode] =
            /*must be evaluated when all 'correspondingNodes' are set to
            'unExplored'
             */
            //val postOrdReversed: Stream[IsExploredNode] =
            /*Precumably fails here
            java.lang.StackOverflowError
          at scala.collection.immutable.Stream$Cons.tail
             */
            val postOrdReversed: List[IsExploredNode] =
            //def postOrdReversed: List[IsExploredNode] = {
            /*reset 'nodes' as `unExplored`*/
            /*can mutate existing collection or create new with 'def'*/
            //val reSetedNodes: Vector[IsExploredNode] =
            /*side effect*/
            /*correspondingNodes
            .map(_.isExplored = false)
            println(
                     s"\n'correspondingNodes' are reset as `unExplored`")*/
            /*return value*/
              DepthFirstPostOrder(
                                   graph =
                                     explorableAdjacencyList,
                                   graphLength =
                                     explorableAdjacencyList
                                     .length
                                 )
              //.view
              ////.reverse
              //.toStream
              .toList
            //}

            /*println(
                     s"\n'postOrdReversed' defined")*/

            //first 15 'nodes' in 'preOrd` are:
            //1[e],7[e],5[e],10[e],13[e],11[e],12[e],2[e],6[e],3[e],4[e],
            // 8[e],9[e]
            //5[e],13[e],11[e],12[e],10[e],7[e],2[e],6[e],1[e],4[e],3[e],
            // 8[e],9[e]
            //first 15 'nodes' in 'postOrd` are:
            /*println(
                     s"\n'preOrd.length` is:${
                       preOrd.length
                     }" +
                       s"\nfirst $takeNumber 'nodes' in 'preOrd` are:" +
                       s"\n${
                         preOrd
                         .take(takeNumber)
                         .mkString(",")
                       }"
                   )*/
            /*println(
                     s"\n'postOrd.head` is:${
                       postOrd.head
                     }"*/
            /*s"\n'postOrd.length` is:${
              postOrd.length
            }" +
              s"\nfirst $takeNumber 'nodes' in 'postOrd` are:" +
              s"\n${
                postOrd
                .take(takeNumber)
                .mkString(",")
              }"*/
            //)

            /*reset 'nodes' as `unExplored`*/
            /*can mutate existing collection or create new with 'def'*/
            //val reSetedNodes: Vector[IsExploredNode] =
            correspondingNodes
            .map(_.isExplored = false)
            println(
                     s"\n'correspondingNodes' are reset as `unExplored`")

            /*!!!Warn: 'arcs' must be sorted by 'arcTail'!!!*/
            val transposeAdjacencyList: Vector[ExplorableNodeWithAdjusted] =
              makeExplorableAdjacencyListFromArcs(
                                                   nodes =
                                                     correspondingNodes
                                                     .toVector,
                                                   /*?memory leak ?*/
                                                   /*nodesRemains =
                                                     correspondingNodes
                                                     .toStream,*/
                                                   //.toList,
                                                   currentNodeVal =
                                                     minNodeVal - 1,
                                                   arcsRemains =
                                                     //*arcs
                                                     transposedArcs
                                                     .view
                                                     //*mockUpSCCwith4PartsArcs
                                                     //transposeSCCwith4PartsArcs
                                                     //.take(inputTakeNumber)
                                                     .toStream,
                                                   //.toList,
                                                   minNodeVal = minNodeVal,
                                                   maxNodeVal = maxNodeVal,
                                                   //nodesAmount = maxNodeVal,
                                                   nodeIndexShift = -1
                                                 )

            println(
                     s"\n'transposeAdjacencyList` evaluated" /*+
                   s"\n'transposeAdjacencyList` became:" +
                     s"\n'transposeAdjacencyList.head` is:${
                       transposeAdjacencyList.head
                     }"*/
                     /*+
                                         s"\n'explorableAdjacencyList.tail
                                         .head' is ${
                                           transposeAdjacencyList.tail.head
                                         }"*/
                     /*+
                     s"\nfirst $takeNumber in 'transposeAdjacencyList` are:" +
                     s"\n${
                       transposeAdjacencyList
                       .take(takeNumber)
                       .mkString("\n")
                     }"*/
                   )
            /*Reverse `postOrder` needed*/
            //val graphSCCs: List[List[IsExploredNode]] =
            //val graphSCCsSizes: List[Int] =
            //*val graphSCCsSizes: Stream[Int] =
            val graphSCCsTopFiveSizes: Stream[Int] =
              transposeDepthFirstOrderSCCsSize(
                                                graph =
                                                  transposeAdjacencyList,
                                                postOrderRemains =
                                                  postOrdReversed //,
                                                //postOrd
                                                //preOrd
                                                //.reverse,
                                                /*graphLength =
                                                  transposeAdjacencyList
                                                  .length*/
                                              )
              //.sorted
              //.reverse
              .view
              .sorted(Ordering[Int].reverse)
              .take(5)
              .toStream

            /*println(
                     s"\n'graphSCCsSizes.length` is:${graphSCCsSizes.length}" +
                       s"\nfirst $takeNumber SCCs in 'graphSCCsSizes` are:" +
                       s"\n${
                         graphSCCsSizes
                         .take(takeNumber)
                         .mkString("\n")
                       }"
                   )*/
            println(
                     s"\n`graphSCCsTopFiveSizes` are:" +
                       s"\n${
                         graphSCCsTopFiveSizes
                         .mkString("\n")
                       }"
                   )

            assume(
                    //true == true,
                    graphSCCsTopFiveSizes.nonEmpty, //&&
                    //graphSCCsSizes.length == expectedSize,
                    s"\n'graphSCCsTopFiveSizes' must be 'nonEmpty' " +
                      s"& equal to 'expectedSize'"
                  )
          }
  ignore(
          "51: 'BFS_SCC_NodesAmountOptimized'" +
            "should " +
            "return exact amount of 'nodes' in `connected component`"
        ) {
            val mockUpSCCwith4PartsArcs: Vector[Arc] =
              Vector(
                      //size = 2
                      Arc(1, 2),
                      Arc(2, 3),
                      Arc(3, 1),
                      //size =2
                      Arc(4, 2),
                      Arc(4, 3),
                      Arc(4, 5),
                      Arc(5, 4),
                      Arc(5, 6),
                      //size = 2
                      Arc(6, 3),
                      Arc(6, 7),
                      Arc(7, 6),
                      //size = 1
                      Arc(8, 5),
                      Arc(8, 7),
                      Arc(8, 8)
                    )
            val correspondingNodes: Vector[IsExploredNode] =
              (1 to 8)
              .map(IsExploredNode(_, false))
              .toVector
            val takeNumber: Int = 5
            val nodesLimit: Int = 875714
            val expectedSize: Int =
              6
            val startingNode: Int =
              1
            val expectedNodesInSCC: Int = 1
            val filePath: String =
              "/media/gluk-alex/" +
                "GDI/Java/Scala/sbt/projects/" +
                "stanfordAlgorithmsDesignAndAnalysis1/" +
                "src/test/scala/" +
                "testSCC/"
            //SCC.txt
            val fileName: String = "SCC.txt"
            val actualFileContent: Iterator[String] =
              Iterator.empty
            /*readFromFile(
                          fileName = fileName,
                          filePath = filePath
                        )*/
            /*val arcs: Vector[Arc] =
              extractArcs(actualFileContent)*/
            val DirectedGraph(nodes, arcs): DirectedGraph =
              extractArcsAndNodes(actualFileContent)
            val mockUpNodes: Vector[IsExploredNode] =
              (1 to 6)
              .map(IsExploredNode(_, false))
              .toVector
            val mockUpGraphWith3CC: Vector[ArcFromNodes] =
              Vector(
                      ArcFromNodes(mockUpNodes(1 - 1), mockUpNodes(2 - 1)),
                      ArcFromNodes(mockUpNodes(3 - 1), mockUpNodes(5 - 1)),
                      ArcFromNodes(mockUpNodes(3 - 1), mockUpNodes(4 - 1)),
                      ArcFromNodes(mockUpNodes(4 - 1), mockUpNodes(5 - 1)),
                      ArcFromNodes(mockUpNodes(6 - 1), mockUpNodes(6 - 1))
                    )

            /*println(
                     s"\n'mockUpGraphWith3CC.length` is:${
                       mockUpGraphWith3CC.length
                     }" +
                       s"\nfirst $takeNumber from 'mockUpGraphWith3CC` are:" +
                       s"\n${
                         mockUpGraphWith3CC
                         .take(takeNumber)
                         .mkString("\n")
                       }"
                   )*/
            /*println(
                     s"\n'arcs.head` is:${arcs.head}"
                   )*/
            /*println(
                     s"\n'arcs.length` is:${arcs.length}" +
                       s"\nfirst $takeNumber 'arcs` are:" +
                       s"\n${
                         arcs
                         .take(takeNumber)
                         .mkString("\n")
                       }"
                   )*/


            val unExploredNodes: Vector[IsExploredNode] =
              correspondingNodes
            //mockUpNodes
            //(1 to 8)
            /*nodes
            .map(IsExploredNode(_, false))
            .toVector*/

            /*println(
                     s"\n'unExploredNodes.head` is:${unExploredNodes.head}"
                   )*/
            println(
                     s"\n'startingNode` is:${startingNode}"
                   )

            /*?too slow on big input?*/
            //val unExploredArcs: Seq[ArcFromNodes] =
            val unExploredArcs: Vector[ArcFromNodes] =
            //mockUpGraphWith3CC
              setArcsUnExplored(
                                 nodes = unExploredNodes,
                                 arcsRemain =
                                   //arcs
                                   mockUpSCCwith4PartsArcs
                               )

            println(
                     s"\n'unExploredArcs.length` is:${unExploredArcs.length}" +
                       s"\nfirst $takeNumber 'unExploredArcs` are:" +
                       s"\n${
                         unExploredArcs
                         .take(takeNumber)
                         .mkString("\n")
                       }"
                   )
            val nodesInSCC: Int =
              BFS_SCC_NodesAmountOptimized(
                                            graph =
                                              unExploredArcs,
                                            //mockUpGraphWith3CC,
                                            startingNode = startingNode
                                          )

            println(
                     s"\n'nodesInSCC` is:${nodesInSCC}")
            assume(
                    //true == true,
                    nodesInSCC == expectedNodesInSCC,
                    s"\n'nodesInSCC' must be 'nonEmpty' & " +
                      s"equal to 'expectedNodesInSCC'"
                  )
          }
  ignore(
          "52: 'findAllCCwithBFSOptimized'" +
            "should " +
            "return exact amount of 'CC'" +
            "with right amount of 'nodes' in `connected component`"
        ) {
            val mockUpSCCwith4PartsArcs: Vector[Arc] =
              Vector(
                      //size = 2
                      Arc(1, 2),
                      Arc(2, 3),
                      Arc(3, 1),
                      //size =2
                      Arc(4, 2),
                      Arc(4, 3),
                      Arc(4, 5),
                      Arc(5, 4),
                      Arc(5, 6),
                      //size = 2
                      Arc(6, 3),
                      Arc(6, 7),
                      Arc(7, 6),
                      //size = 1
                      Arc(8, 5),
                      Arc(8, 7),
                      Arc(8, 8)
                    )
            val correspondingNodes: Vector[IsExploredNode] =
              (1 to 8)
              .map(IsExploredNode(_, false))
              .toVector
            val takeNumber: Int = 5
            val nodesLimit: Int = 875714
            val expectedSize: Int =
              4
            val startingNode: Int =
              1
            val expectedNodesInSCC: Int = 1
            val expectedNumberOfCCs: Int = 4
            val filePath: String =
              "/media/gluk-alex/" +
                "GDI/Java/Scala/sbt/projects/" +
                "stanfordAlgorithmsDesignAndAnalysis1/" +
                "src/test/scala/" +
                "testSCC/"
            //SCC.txt
            val fileName: String = "SCC.txt"
            val actualFileContent: Iterator[String] =
              Iterator.empty
            /*readFromFile(
                          fileName = fileName,
                          filePath = filePath
                        )*/
            /*val arcs: Vector[Arc] =
              extractArcs(actualFileContent)*/
            val DirectedGraph(nodes, arcs): DirectedGraph =
              extractArcsAndNodes(actualFileContent)
            val mockUpNodes: Vector[IsExploredNode] =
              (1 to 6)
              .map(IsExploredNode(_, false))
              .toVector
            val mockUpGraphWith3CC: Vector[ArcFromNodes] =
              Vector(
                      ArcFromNodes(mockUpNodes(1 - 1), mockUpNodes(2 - 1)),
                      ArcFromNodes(mockUpNodes(3 - 1), mockUpNodes(5 - 1)),
                      ArcFromNodes(mockUpNodes(3 - 1), mockUpNodes(4 - 1)),
                      ArcFromNodes(mockUpNodes(4 - 1), mockUpNodes(5 - 1)),
                      ArcFromNodes(mockUpNodes(6 - 1), mockUpNodes(6 - 1))
                    )

            /*println(
                     s"\n'mockUpGraphWith3CC.length` is:${
                       mockUpGraphWith3CC.length
                     }" +
                       s"\nfirst $takeNumber from 'mockUpGraphWith3CC` are:" +
                       s"\n${
                         mockUpGraphWith3CC
                         .take(takeNumber)
                         .mkString("\n")
                       }"
                   )*/
            /*println(
                     s"\n'arcs.head` is:${arcs.head}"
                   )*/
            /*println(
                     s"\n'arcs.length` is:${arcs.length}" +
                       s"\nfirst $takeNumber 'arcs` are:" +
                       s"\n${
                         arcs
                         .take(takeNumber)
                         .mkString("\n")
                       }"
                   )*/


            val unExploredNodes: Vector[IsExploredNode] =
              correspondingNodes
            //mockUpNodes
            //(1 to 8)
            /*nodes
            .map(IsExploredNode(_, false))
            .toVector*/

            /*println(
                     s"\n'unExploredNodes.head` is:${unExploredNodes.head}"
                   )*/
            println(
                     s"\n'startingNode` is:${startingNode}"
                   )

            /*?too slow on big input?*/
            //val unExploredArcs: Seq[ArcFromNodes] =
            val unExploredArcs: Vector[ArcFromNodes] =
            //mockUpGraphWith3CC
              setArcsUnExplored(
                                 nodes = unExploredNodes,
                                 arcsRemain =
                                   //arcs
                                   mockUpSCCwith4PartsArcs
                               )

            val cCs: Seq[Int] =
              findAllCCwithBFSOptimized(
                                         graph =
                                           unExploredArcs,
                                         graphNodes =
                                           unExploredNodes
                                           .toArray,
                                         nodesLimit =
                                           unExploredNodes.length
                                       )
            println(
                     s"\n'cCs.length` is:${cCs.length}" +
                       s"\nfirst $takeNumber 'unExploredArcs` are:" +
                       s"\n${
                         cCs
                         .take(takeNumber)
                         .mkString("\n")
                       }"
                   )
            assume(
                    //true == true,
                    cCs == expectedNumberOfCCs,
                    s"\n'cCs' must be 'nonEmpty' & " +
                      s"equal to 'expectedNumberOfCCs'"
                  )
          }
  ignore(
          "60: 'makeAdjacencyListMapFromArcs' " +
            "should return " +
            "'AdjacencyListMap'"
        ) {
            val sourceSize: Int = 5105043
            val expectedNodesSize: Int = 875714
            val expectedArcsSize: Int = 5105043
            /*all 'nodes', but only few / some 'arcs'*/
            /*at least as big as `mockUp(14)`*/
            val inputTakeNumber: Int = 20
            val expectedNodesInSCC: Int = 3
            val expectedSize: Int = 4
            val filePath: String =
              "/home/gluk-alex/Documents/sbt_projects/"
            /*"/media/gluk-alex/" +
              "GDI/Java/Scala/sbt/projects/" +
              "stanfordAlgorithmsDesignAndAnalysis1/" +
              "src/test/scala/" +
              "testSCC/"*/
            val fileName: String =
            //"tinyDG.txt"
              "SCC.txt"
            val actualFileContent: Iterator[String] =
            //Iterator.empty
              readFromFile(
                            fileName = fileName,
                            filePath = filePath
                          )
            val nodesInGraph: Int =
            //actualFileContent.next().toInt
              875714
            val edgesInGraph: Int =
            //actualFileContent.next().toInt
              5105043
            println(
                     s"\ntotal 'nodesInGraph`:$nodesInGraph" +
                       s"\ntotal 'edgesInGraph`:$edgesInGraph"
                   )
            val arcs: Vector[Arc] =
            //extractSortedArcs(actualFileContent)
              Vector.empty
              .view
              /*reduce / control input size*/
              .take(inputTakeNumber)
              .toVector

            println(
                     s"\n'arcs` are extracted from file" /*+
                   s"\n'arcs.head` is:${arcs.head}" +
                     s"\n'arcs.tail.head' is: ${
                       arcs.tail.head
                     }"*/
                   )

            /*reversed directions*/
            val transposedArcs: Vector[Arc] =
              arcs
              .view
              .map(a => Arc(a.arcHead, a.arcTail))
              .sortBy(_.arcTail)
              .toVector
            val correspondingNodes: Vector[IsExploredNode] =
            /*create new collection as `unexplored`*/
              Vector.empty
            val startingNode: Int = 1
            val takeNumber: Int = 15
            val nodesLimit: Int = Int.MinValue

            lazy val startTime: java.util.Date = Calendar.getInstance()
                                                 .getTime()
            lazy val timeStamp1: Long = System.currentTimeMillis
            /*Date and Time Pattern */
            val timeStampFormat = new SimpleDateFormat("HH:mm:ss.SSS")
            lazy val startStampString = timeStampFormat.format(startTime)
            println(s"'fillDiGraphArrayWithArcs' started at:" +
                      startStampString)
            println(s"timeStamp1:" + timeStamp1)
            //println

            /*!!!Warn: 'arcs' must be sorted by 'arcTail'!!!*/
            val mapWithAdjacencyList: Map[Int, NodeMapValFieldsStatic] =
              makeAdjacencyListMapFromArcs(
                                            fileContentIter = actualFileContent,
                                            pattern =
                                              """\d+""".r
                                          )
            lazy val endTime = Calendar.getInstance().getTime()
            lazy val timeStamp2: Long = System.currentTimeMillis()
            lazy val endStampString = timeStampFormat.format(endTime)
            val timeDifference: Long =
              timeStamp2 - timeStamp1
            println(s"Done at:" + endStampString)
            println(s"timeStamp2:" + timeStamp2)
            println(s"time difference is:" +
                      (timeStamp2 - timeStamp1) + " Millis")
            println(s"time difference is:" +
                      convertLongToTimeString(
                                               timeNumberMillis =
                                                 timeDifference,
                                               colored = false
                                             )
                   )

            println(
                     s"Max 'adjustedNodes.size' in 'mapWithAdjacencyList' is:" +
                       mapWithAdjacencyList
                       //.values
                       //.valuesIterator
                       /*view warper of original map return key->f(value)*/
                       .mapValues(_.adjustedNodes.size)
                       .values
                       //.view
                       .max
                     //.min
                     //.headOption
                     /*+
                 s"'mapWithAdjacencyList.head' is:" +
                   mapWithAdjacencyList.head +
                   s"\n'mapWithAdjacencyList.tail.head' is ${
                     mapWithAdjacencyList.tail.head
                   }" +
                   s"\n'mapWithAdjacencyList` is:" +
                   s"\nfirst $takeNumber elements in " +
                   s"'mapWithAdjacencyList`" +
                   s" are:" +
                   s"\n${
                     mapWithAdjacencyList
                     .take(takeNumber)
                     .mkString("\n")
                   }"*/
                   )

            assume(
                    //true == true,
                    mapWithAdjacencyList.nonEmpty &&
                      mapWithAdjacencyList.size == nodesInGraph,
                    s"\n'graphSCCsTopFiveSizes' must be 'nonEmpty' " +
                      s"& equal to 'expectedSize'"
                  )
          }
  ignore(
          "61: 'fillDirectedGraphDynamicFromArcs' " +
            "should return " +
            "'new DirectedGraphDynamic'"
        ) {
            val sourceSize: Int = 5105043
            val expectedNodesSize: Int = 875714
            val expectedArcsSize: Int = 5105043
            /*all 'nodes', but only few / some 'arcs'*/
            /*at least as big as `mockUp(14)`*/
            val inputTakeNumber: Int = 20
            val expectedNodesInSCC: Int = 3
            val expectedSize: Int = 4
            val filePath: String =
              "/media/gluk-alex/" +
                "GDI/Java/Scala/sbt/projects/" +
                "stanfordAlgorithmsDesignAndAnalysis1/" +
                "src/test/scala/" +
                "testSCC/"
            //SCC.txt
            val fileName: String = "tinyDG.txt"
            val actualFileContent: Iterator[String] =
            //Iterator.empty
              readFromFile(
                            fileName = fileName,
                            filePath = filePath
                          )
            val nodesInGraph: Int =
              actualFileContent.next().toInt
            val edgesInGraph: Int =
              actualFileContent.next().toInt
            println(
                     s"\ntotal 'nodesInGraph`:$nodesInGraph" +
                       s"\ntotal 'edgesInGraph`:$edgesInGraph"
                   )
            val arcs: Vector[Arc] =
            //extractSortedArcs(actualFileContent)
              Vector.empty
              .view
              /*reduce / control input size*/
              .take(inputTakeNumber)
              .toVector

            println(
                     s"\n'arcs` are extracted from file" /*+
                   s"\n'arcs.head` is:${arcs.head}" +
                     s"\n'arcs.tail.head' is: ${
                       arcs.tail.head
                     }"*/
                   )

            /*reversed directions*/
            val transposedArcs: Vector[Arc] =
              arcs
              .view
              .map(a => Arc(a.arcHead, a.arcTail))
              .sortBy(_.arcTail)
              .toVector
            val correspondingNodes: Vector[IsExploredNode] =
            /*create new collection as `unexplored`*/
              Vector.empty
            val startingNode: Int = 1
            val takeNumber: Int = 15
            val nodesLimit: Int = Int.MinValue

            val directedGraphDynamic: DirectedGraphDynamic =
              fillDirectedGraphDynamicFromArcs(
                                                fileContentIter =
                                                  actualFileContent,
                                                pattern =
                                                  """\d+""".r
                                              )
            println(
                     s"\n'directedGraphDynamic.nodesWithAdjusted.size' is:" +
                       directedGraphDynamic.nodesWithAdjusted.size +
                       /*s"\n'mapWithAdjacencyList.head' is:" +
                         directedGraphDynamic
                           .nodesWithAdjusted
                         .head +
                         s"\n'mapWithAdjacencyList.tail.head' is ${
                           directedGraphDynamic
                           .nodesWithAdjusted
                           .tail.head
                         }" +*/
                       //s"\n'nodesWithAdjusted` is:" +
                       s"\nfirst $takeNumber elements in " +
                       s"'nodesWithAdjusted`" +
                       s" are:" +
                       s"\n${
                         directedGraphDynamic
                         .nodesWithAdjusted
                         .take(takeNumber)
                         .mkString("\n")
                       }"
                   )

            assume(
                    //true == true,
                    directedGraphDynamic
                    .nodesWithAdjusted
                    .nonEmpty &&
                      directedGraphDynamic
                      .nodesWithAdjusted
                      .size == nodesInGraph,
                    s"\n'directedGraphDynamic' must be 'nonEmpty' " +
                      s"& equal to 'nodesInGraph'"
                  )
          }
  ignore(
          "62: 'pre_PostOrderDFS_Map' " +
            "should return " +
            "right reachable 'nodes' ordering"
        ) {
            val sourceSize: Int = 5105043
            val expectedNodesSize: Int = 875714
            val expectedArcsSize: Int = 5105043
            /*all 'nodes', but only few / some 'arcs'*/
            /*at least as big as `mockUp(14)`*/
            val inputTakeNumber: Int = 2000
            val expectedNodesInSCC: Int = 3
            val expectedSize: Int = 4
            val filePath: String =
              "/media/gluk-alex/" +
                "GDI/Java/Scala/sbt/projects/" +
                "stanfordAlgorithmsDesignAndAnalysis1/" +
                "src/test/scala/" +
                "testSCC/"
            //SCC.txt
            val fileName: String =
              "tinyDG.txt"
            //"SCC.txt"
            val actualFileContent: Iterator[String] =
            //Iterator.empty
              readFromFile(
                            fileName = fileName,
                            filePath = filePath
                          )
            /*reduce / control input size*/
            //.take(inputTakeNumber)
            println(
                     s"\n'actualFileContent` is:$actualFileContent"
                   )

            val nodesInGraph: Int =
              actualFileContent.next().toInt
            val edgesInGraph: Int =
              actualFileContent.next().toInt
            println(
                     s"\ntotal 'nodesInGraph`:$nodesInGraph" +
                       s"\ntotal 'edgesInGraph`:$edgesInGraph"
                   )

            val arcs: Vector[Arc] =
            //extractSortedArcs(actualFileContent)
              Vector.empty
              .view
              /*reduce / control input size*/
              //.take(inputTakeNumber)
              .toVector

            println(
                     s"\n'arcs` are extracted from file" /*+
                   s"\n'arcs.head` is:${arcs.head}" +
                     s"\n'arcs.tail.head' is: ${
                       arcs.tail.head
                     }"*/
                   )

            /*reversed directions*/
            val transposedArcs: Vector[Arc] =
              arcs
              .view
              .map(a => Arc(a.arcHead, a.arcTail))
              .sortBy(_.arcTail)
              .toVector
            val correspondingNodes: Vector[IsExploredNode] =
            /*create new collection as `unexplored`*/
              Vector.empty
            val startingNode: Int =
            //9
            //1 //*sink
            //12
            //6
            //7
            // 8
              0
            val takeNumber: Int = 15
            val nodesLimit: Int = Int.MinValue

            /*!!!Warn: 'arcs' must be sorted by 'arcTail'!!!*/
            val mapWithAdjacencyList: Map[Int, NodeMapValFieldsStatic] =
              makeAdjacencyListMapFromArcs(
                                            fileContentIter =
                                              actualFileContent
                                            /*.take(inputTakeNumber)*/ ,
                                            pattern =
                                              """\d+""".r
                                          )
            println(
                     s"\n'mapWithAdjacencyList.isEmpty' is:" +
                       mapWithAdjacencyList.isEmpty +
                       s"\n'mapWithAdjacencyList.size' is:" +
                       mapWithAdjacencyList.size +
                       s"\n'mapWithAdjacencyList.get($startingNode)' is:" +
                       mapWithAdjacencyList.get(startingNode) /*+
                     s"\n'mapWithAdjacencyList.head' is:" +
                     mapWithAdjacencyList.headOption +
                     s"\n'mapWithAdjacencyList.tail.head' is ${
                       mapWithAdjacencyList.tail.head
                     }" +
                     s"\n'mapWithAdjacencyList` is:" +
                     s"\nfirst $takeNumber elements in 'mapWithAdjacencyList`" +
                     s" are:" +
                     s"\n${
                       mapWithAdjacencyList
                       .take(takeNumber)
                       .mkString("\n")
                     }"*/
                   )
            val DFSResults(
            preExplored,
            postExplored,
            totalNodesFound):
            DFSResults =
            //pre_PostOrderDFS_ver3(
            /*TODO DeBug*/
              pre_PostOrderDFS_ver4(
                                     graph = mapWithAdjacencyList,
                                     startNodeKey = startingNode
                                   )
            /*val resultSetDFS: BitSet =
              pre_PostOrderDFS_ver2(
                                     graph = mapWithAdjacencyList,
                                     startNode = startingNode
                                   )*/
            println(
                     s"\n'totalNodesFound`:" + totalNodesFound +
                       s"\nfirst $takeNumber elements in 'preExplored`" +
                       s" are:" +
                       s"\n${
                         preExplored
                         .take(takeNumber)
                         .mkString(",")
                       }" +
                       s"\n'postExplored` is:" +
                       s"\nfirst $takeNumber elements in 'postExplored`" +
                       s" are:" +
                       s"\n${
                         postExplored
                         .take(takeNumber)
                         .mkString(",")
                       }"
                   )
            /*println(
                     s"\nis 'resultSetDFS.nonEmpty`:${resultSetDFS.nonEmpty}" +
                     s"\n'resultSetDFS.size` is:${resultSetDFS.size}" /*+
                       s"\nfirst $takeNumber elements in 'resultSetDFS`" +
                       s" are:" +
                       s"\n${
                         resultSetDFS
                         .take(takeNumber)
                         .mkString(",")
                       }"*/
                   )*/

            assume(
                    //true == true,
                    //postExplored.nonEmpty &&
                    totalNodesFound > 0
                    /*resultSetDFS.nonEmpty &&
                      resultSetDFS.size == nodesInGraph*/ ,
                    s"\n'postExplored' must be 'nonEmpty' " +
                      s"& equal to 'nodesInGraph'"
                  )
          }
  ignore(
          "63: 'nodesCounterDFS' " +
            "should return " +
            "right reachable number of 'nodes' in graph"
        ) {
            val sourceSize: Int = 5105043
            val expectedNodesSize: Int = 875714
            val expectedArcsSize: Int = 5105043
            /*all 'nodes', but only few / some 'arcs'*/
            /*at least as big as `mockUp(14)`*/
            val inputTakeNumber: Int = 2000
            val expectedNodesInSCC: Int = 3
            val expectedSize: Int = 4
            val filePath: String =
              "/media/gluk-alex/" +
                "GDI/Java/Scala/sbt/projects/" +
                "stanfordAlgorithmsDesignAndAnalysis1/" +
                "src/test/scala/" +
                "testSCC/"
            //SCC.txt
            val fileName: String =
              "tinyDG.txt"
            //"SCC.txt"
            val actualFileContent: Iterator[String] =
            //Iterator.empty
              readFromFile(
                            fileName = fileName,
                            filePath = filePath
                          )
            /*reduce / control input size*/
            //.take(inputTakeNumber)
            println(
                     s"\n'actualFileContent` is:$actualFileContent"
                   )

            val nodesInGraph: Int =
              actualFileContent.next().toInt
            val edgesInGraph: Int =
              actualFileContent.next().toInt
            println(
                     s"\ntotal 'nodesInGraph`:$nodesInGraph" +
                       s"\ntotal 'edgesInGraph`:$edgesInGraph"
                   )

            val arcs: Vector[Arc] =
            //extractSortedArcs(actualFileContent)
              Vector.empty
              .view
              /*reduce / control input size*/
              //.take(inputTakeNumber)
              .toVector

            println(
                     s"\n'arcs` are extracted from file" /*+
                   s"\n'arcs.head` is:${arcs.head}" +
                     s"\n'arcs.tail.head' is: ${
                       arcs.tail.head
                     }"*/
                   )

            /*reversed directions*/
            val transposedArcs: Vector[Arc] =
              arcs
              .view
              .map(a => Arc(a.arcHead, a.arcTail))
              .sortBy(_.arcTail)
              .toVector
            val correspondingNodes: Vector[IsExploredNode] =
            /*create new collection as `unexplored`*/
              Vector.empty
            val startingNode: Int =
            //1
            //6
            //7 //sink
            // 8
              0
            val takeNumber: Int = 15
            val nodesLimit: Int = Int.MinValue

            /*!!!Warn: 'arcs' must be sorted by 'arcTail'!!!*/
            val mapWithAdjacencyList: Map[Int, NodeMapValFieldsStatic] =
              makeAdjacencyListMapFromArcs(
                                            fileContentIter =
                                              actualFileContent
                                            /*.take(inputTakeNumber)*/ ,
                                            pattern =
                                              """\d+""".r,
                                            nonReversedArcs = false
                                          )
            println(
                     s"\n'mapWithAdjacencyList.isEmpty' is:" +
                       mapWithAdjacencyList.isEmpty +
                       s"\n'mapWithAdjacencyList.size' is:" +
                       mapWithAdjacencyList.size +
                       s"\n'mapWithAdjacencyList.get(startingNode)' is:" +
                       mapWithAdjacencyList.get(startingNode) /*+
                     s"\n'mapWithAdjacencyList.head' is:" +
                     mapWithAdjacencyList.headOption +
                     s"\n'mapWithAdjacencyList.tail.head' is ${
                       mapWithAdjacencyList.tail.head
                     }" +
                     s"\n'mapWithAdjacencyList` is:"*/ +
                       s"\nfirst $takeNumber elements in " +
                       s"'mapWithAdjacencyList`" +
                       s" are:" +
                       s"\n${
                         mapWithAdjacencyList
                         .take(takeNumber)
                         .mkString(",")
                       }"
                   )
            val totalNodesFound:
            Int =
              nodesCounterDFS(
                               graph = mapWithAdjacencyList,
                               startNode = startingNode
                             )
            /*val resultSetDFS: BitSet =
              pre_PostOrderDFS_ver2(
                                     graph = mapWithAdjacencyList,
                                     startNode = startingNode
                                   )*/
            println(
                     s"\n'totalNodesFound`:" + totalNodesFound
                   )

            assume(
                    //true == true,
                    //postExplored.nonEmpty &&
                    //totalNodesFound > 0
                    totalNodesFound == nodesInGraph,
                    s"\n'totalNodesFound' must be > 0 " +
                      s"& equal to 'nodesInGraph'"
                  )
          }
  ignore(
          "64: 'DFS_Ordering' " +
            "should return " +
            "right ?topological? ordering of all graph 'nodes'"
        ) {
            val sourceSize: Int = 5105043
            val expectedNodesSize: Int = 875714
            val expectedArcsSize: Int = 5105043
            /*all 'nodes', but only few / some 'arcs'*/
            /*at least as big as `mockUp(14)`*/
            val inputTakeNumber: Int = 2000
            val expectedNodesInSCC: Int = 3
            val expectedSize: Int = 4
            val filePath: String =
              "/media/gluk-alex/" +
                "GDI/Java/Scala/sbt/projects/" +
                "stanfordAlgorithmsDesignAndAnalysis1/" +
                "src/test/scala/" +
                "testSCC/"
            //SCC.txt
            val fileName: String =
              "tinyDG.txt"
            //"SCC.txt"
            val actualFileContent: Iterator[String] =
            //Iterator.empty
              readFromFile(
                            fileName = fileName,
                            filePath = filePath
                          )
            /*reduce / control input size*/
            //.take(inputTakeNumber)
            println(
                     s"\n'actualFileContent` is:$actualFileContent"
                   )

            val nodesInGraph: Int =
              actualFileContent.next().toInt
            val edgesInGraph: Int =
              actualFileContent.next().toInt
            println(
                     s"\ntotal 'nodesInGraph`:$nodesInGraph" +
                       s"\ntotal 'edgesInGraph`:$edgesInGraph"
                   )

            val arcs: Vector[Arc] =
            //extractSortedArcs(actualFileContent)
              Vector.empty
              .view
              /*reduce / control input size*/
              //.take(inputTakeNumber)
              .toVector

            println(
                     s"\n'arcs` are extracted from file" /*+
                   s"\n'arcs.head` is:${arcs.head}" +
                     s"\n'arcs.tail.head' is: ${
                       arcs.tail.head
                     }"*/
                   )

            /*reversed directions*/
            val transposedArcs: Vector[Arc] =
              arcs
              .view
              .map(a => Arc(a.arcHead, a.arcTail))
              .sortBy(_.arcTail)
              .toVector
            val correspondingNodes: Vector[IsExploredNode] =
            /*create new collection as `unexplored`*/
              Vector.empty
            val startingNode: Int =
            //1
            //6
              7 //sink
    // 8
    //0
    val takeNumber: Int = 15
            val nodesLimit: Int = Int.MinValue

            /*!!!Warn: 'arcs' must be sorted by 'arcTail'!!!*/
            val mapWithAdjacencyList: Map[Int, NodeMapValFieldsStatic] =
              makeAdjacencyListMapFromArcs(
                                            fileContentIter =
                                              actualFileContent
                                            /*.take(inputTakeNumber)*/ ,
                                            pattern =
                                              """\d+""".r,
                                            nonReversedArcs = false
                                          )
            println(
                     s"\n'mapWithAdjacencyList.isEmpty' is:" +
                       mapWithAdjacencyList.isEmpty +
                       s"\n'mapWithAdjacencyList.size' is:" +
                       mapWithAdjacencyList.size +
                       s"\n'mapWithAdjacencyList.get(startingNode)' is:" +
                       mapWithAdjacencyList.get(startingNode) /*+
                     s"\n'mapWithAdjacencyList.head' is:" +
                     mapWithAdjacencyList.headOption +
                     s"\n'mapWithAdjacencyList.tail.head' is ${
                       mapWithAdjacencyList.tail.head
                     }" +
                     s"\n'mapWithAdjacencyList` is:" +
                     s"\nfirst $takeNumber elements in 'mapWithAdjacencyList`" +
                     s" are:" +
                     s"\n${
                       mapWithAdjacencyList
                       .take(takeNumber)
                       .mkString("\n")
                     }"*/
                   )
            val DFSResults(
            preExplored,
            postExplored,
            totalNodesFound):
            DFSResults =
              DFS_Ordering(
                            graph = mapWithAdjacencyList,
                            mapKeyIter =
                              mapWithAdjacencyList
                              .keysIterator
                          )
            /*val resultSetDFS: BitSet =
              pre_PostOrderDFS_ver2(
                                     graph = mapWithAdjacencyList,
                                     startNode = startingNode
                                   )*/
            println(
                     s"\n'totalNodesFound`:" + totalNodesFound +
                       s"\nfirst $takeNumber elements in 'preExplored`" +
                       s" are:" +
                       s"\n${
                         preExplored
                         .take(takeNumber)
                         .mkString(",")
                       }" +
                       s"\n'postExplored` is:" +
                       s"\nfirst $takeNumber elements in 'postExplored`" +
                       s" are:" +
                       s"\n${
                         postExplored
                         .take(takeNumber)
                         .mkString(",")
                       }"
                   )
            /*println(
                     s"\nis 'resultSetDFS.nonEmpty`:${resultSetDFS.nonEmpty}" +
                     s"\n'resultSetDFS.size` is:${resultSetDFS.size}" /*+
                       s"\nfirst $takeNumber elements in 'resultSetDFS`" +
                       s" are:" +
                       s"\n${
                         resultSetDFS
                         .take(takeNumber)
                         .mkString(",")
                       }"*/
                   )*/

            assume(
                    //true == true,
                    //postExplored.nonEmpty &&
                    totalNodesFound > 0
                    /*resultSetDFS.nonEmpty &&
                      resultSetDFS.size == nodesInGraph*/ ,
                    s"\n'postExplored' must be 'nonEmpty' " +
                      s"& equal to 'nodesInGraph'"
                  )
          }
  ignore(
          "65: 'DFS_SCCs_Size' " +
            "should return " +
            "amount equal to number of all graph 'nodes'"
        ) {
            val sourceSize: Int = 5105043
            val expectedNodesSize: Int = 875714
            val expectedArcsSize: Int = 5105043
            /*all 'nodes', but only few / some 'arcs'*/
            /*at least as big as `mockUp(14)`*/
            val inputTakeNumber: Int = 2000
            val expectedNodesInSCC: Int = 3
            val expectedSize: Int = 4
            val filePath: String =
              "/media/gluk-alex/" +
                "GDI/Java/Scala/sbt/projects/" +
                "stanfordAlgorithmsDesignAndAnalysis1/" +
                "src/test/scala/" +
                "testSCC/"
            //SCC.txt
            val fileName: String =
              "tinyDG.txt"
            //"SCC.txt"
            val actualFileContent: Iterator[String] =
            //Iterator.empty
              readFromFile(
                            fileName = fileName,
                            filePath = filePath
                          )
            /*reduce / control input size*/
            //.take(inputTakeNumber)
            println(
                     s"\n'actualFileContent` is:$actualFileContent"
                   )

            val nodesInGraph: Int =
              actualFileContent.next().toInt
            val edgesInGraph: Int =
              actualFileContent.next().toInt
            println(
                     s"\ntotal 'nodesInGraph`:$nodesInGraph" +
                       s"\ntotal 'edgesInGraph`:$edgesInGraph"
                   )

            val arcs: Vector[Arc] =
            //extractSortedArcs(actualFileContent)
              Vector.empty
              .view
              /*reduce / control input size*/
              //.take(inputTakeNumber)
              .toVector

            println(
                     s"\n'arcs` are extracted from file" /*+
                   s"\n'arcs.head` is:${arcs.head}" +
                     s"\n'arcs.tail.head' is: ${
                       arcs.tail.head
                     }"*/
                   )

            /*reversed directions*/
            val transposedArcs: Vector[Arc] =
              arcs
              .view
              .map(a => Arc(a.arcHead, a.arcTail))
              .sortBy(_.arcTail)
              .toVector
            val correspondingNodes: Vector[IsExploredNode] =
            /*create new collection as `unexplored`*/
              Vector.empty
            val startingNode: Int =
            //1
            //6
              7 //sink
    // 8
    //0
    val takeNumber: Int = 15
            val nodesLimit: Int = Int.MinValue

            val mapWithAdjacencyList: Map[Int, NodeMapValFieldsStatic] =
              makeAdjacencyListMapFromArcs(
                                            fileContentIter =
                                              actualFileContent
                                            /*.take(inputTakeNumber)*/ ,
                                            pattern =
                                              """\d+""".r,
                                            nonReversedArcs = true
                                          )
            println(
                     s"\n'mapWithAdjacencyList.isEmpty' is:" +
                       mapWithAdjacencyList.isEmpty)
            val transposedMapWithAdjacencyList: Map[Int,
              NodeMapValFieldsStatic] =
              makeAdjacencyListMapFromArcs(
                                            fileContentIter =
                                              readFromFile(
                                                            fileName = fileName,
                                                            filePath = filePath
                                                          )
                                              /*skip redundant*/
                                              .drop(2),
                                            pattern =
                                              """\d+""".r,
                                            nonReversedArcs = false
                                          )
            println(
                     s"\n'transposedMapWithAdjacencyList.isEmpty' is:" +
                       transposedMapWithAdjacencyList.isEmpty +
                       s"\n'transposedMapWithAdjacencyList.size' is:" +
                       transposedMapWithAdjacencyList.size /*+
                       s"\n'mapWithAdjacencyList.get(startingNode)' is:" +
                       transposedMapWithAdjacencyList.get(startingNode) */ +
                       s"\n'transposedMapWithAdjacencyList.head' is:" +
                       transposedMapWithAdjacencyList.headOption /*+
                     s"\n'mapWithAdjacencyList.tail.head' is ${
                       mapWithAdjacencyList.tail.head
                     }" +
                     s"\n'mapWithAdjacencyList` is:" +
                     s"\nfirst $takeNumber elements in 'mapWithAdjacencyList`" +
                     s" are:" +
                     s"\n${
                       mapWithAdjacencyList
                       .take(takeNumber)
                       .mkString("\n")
                     }"*/
                   )
            val DFSResults(
            preExplored,
            postExplored,
            totalNodesFound):
            DFSResults =
              DFS_Ordering(
                            graph =
                              transposedMapWithAdjacencyList,
                            mapKeyIter =
                              transposedMapWithAdjacencyList
                              .keysIterator
                          )
            /*val resultSetDFS: BitSet =
              pre_PostOrderDFS_ver2(
                                     graph = mapWithAdjacencyList,
                                     startNode = startingNode
                                   )*/
            println(
                     s"\n'totalNodesFound`:" + totalNodesFound +
                       s"\nfirst $takeNumber elements in 'preExplored`" +
                       s" are:" +
                       s"\n${
                         preExplored
                         .take(takeNumber)
                         .mkString(",")
                       }" +
                       s"\n'postExplored` is:" +
                       s"\nfirst $takeNumber elements in 'postExplored`" +
                       s" are:" +
                       s"\n${
                         postExplored
                         .take(takeNumber)
                         .mkString(",")
                       }"
                   )
            val graphSCCs_Sizes: Stream[Int] =
              DFS_SCCs_Size(
                             graph =
                               mapWithAdjacencyList,
                             mapKeyIter =
                               preExplored
                               .toIterator
                           )
            println(
                     s"\nis 'graphSCCs_Sizes.nonEmpty`:${
                       graphSCCs_Sizes.nonEmpty
                     }" +
                       s"\n'graphSCCs_Sizes.size` is:${graphSCCs_Sizes.size}" +
                       s"\nfirst $takeNumber elements in 'graphSCCs_Sizes`" +
                       s" are:" +
                       s"\n${
                         graphSCCs_Sizes
                         .take(takeNumber)
                         .mkString(",")
                       }"
                   )

            assume(
                    //true == true,
                    //postExplored.nonEmpty &&
                    //totalNodesFound > 0
                    graphSCCs_Sizes.nonEmpty &&
                      graphSCCs_Sizes.size == nodesInGraph,
                    s"\n'postExplored' must be 'nonEmpty' " +
                      s"& equal to 'nodesInGraph'"
                  )
          }
  ignore(
          "66: 'tarjanForDiGraphDyn' " +
            "should return " +
            "all `SCC` in 'DirectedGraphDynamic'"
        ) {
            val sourceSize: Int = 5105043
            val expectedNodesSize: Int = 875714
            val expectedArcsSize: Int = 5105043
            /*all 'nodes', but only few / some 'arcs'*/
            /*at least as big as `mockUp(14)`*/
            val inputTakeNumber: Int =
              250000 +
                //250000 +
                125000 +
                62500 +
                31250 +
                //15625 +
                7812 +
                7812 +
                3906 +
                3906 +
                3906 +
                //3906 +
                //1953 +
                976 +
                //976 +
                488 +
                //488 +
                //244 +
                122 +
                //122 +
                //122
                61 +
                //61 +
                //30 +
                //15 +
                7
            //500000
            val expectedNodesInSCC: Int = 3
            val expectedSCCsInDiGraph: Int = 5 //4
    val expectedSize: Int = 4
            val filePath: String =
              "/media/gluk-alex/" +
                "GDI/Java/Scala/sbt/projects/" +
                "stanfordAlgorithmsDesignAndAnalysis1/" +
                "src/test/scala/" +
                "testSCC/"
            val fileName: String =
              "SCC.txt"
            //"tinyDG.txt"
            //"diGraphWith4SCCs"
            val actualFileContent: Iterator[String] =
            //Iterator.empty
              readFromFile(
                            fileName = fileName,
                            filePath = filePath
                          )
              /*reduce / control input size*/
              .take(inputTakeNumber)
            val nodesInGraph: Int =
            //actualFileContent.next().toInt
              875714
            val edgesInGraph: Int =
            //actualFileContent.next().toInt
              5105043
            println(
                     s"\ntotal 'nodesInGraph`:$nodesInGraph" +
                       s"\ntotal 'edgesInGraph`:$edgesInGraph"
                   )

            val startingNode: Int = 1
            /*only '5' max matter*/
            val takeNumber: Int = 5 //15
    val nodesLimit: Int = Int.MinValue

            val directedGraphDynamic: DirectedGraphDynamic =
              fillDirectedGraphDynamicFromArcs(
                                                fileContentIter =
                                                  actualFileContent,
                                                pattern =
                                                  """\d+""".r
                                              )
            println(
                     s"\n'arcs` are extracted from file" +
                       s"\n'directedGraphDynamic.nodesWithAdjusted.head' is:" +
                       directedGraphDynamic.nodesWithAdjusted.head +
                       s"\n'directedGraphDynamic.nodesWithAdjusted.tail.head'" +
                       s" " +
                       s"is:" +
                       directedGraphDynamic.nodesWithAdjusted.tail.head
                   )

            val allSCCs:
            //Iterable[List[Int]] =
            //List[Int] =
            Stream[Int] =
              tarjanForDiGraphDyn(
                                   directedGraphDynamic.nodesWithAdjusted
                                 )
              .view
              .sorted(Ordering[Int].reverse)
              .take(takeNumber)
              //.toList
              .toStream
            println(
                     s"\n'allSCCs.size' is:" +
                       allSCCs.size +
                       /*s"\n'mapWithAdjacencyList.head' is:" +
                         directedGraphDynamic
                           .nodesWithAdjusted
                         .head +
                         s"\n'mapWithAdjacencyList.tail.head' is ${
                           directedGraphDynamic
                           .nodesWithAdjusted
                           .tail.head
                         }" +*/
                       //s"\n'nodesWithAdjusted` is:" +
                       s"\n`inputTakeNumber` is $inputTakeNumber" +
                       s"\nfirst $takeNumber elements in " +
                       s"'nodesWithAdjusted`" +
                       s" are:" +
                       s"\n${
                         allSCCs
                         .take(takeNumber)
                         .mkString(",")
                       }"
                   )

            assume(
                    //true == true,
                    allSCCs
                    .nonEmpty &&
                      allSCCs
                      .size == expectedSCCsInDiGraph,
                    s"\n'allSCCs' must be 'nonEmpty' " +
                      s"& equal to 'expectedSCCsInDiGraph'"
                  )
          }
  ignore(
          "67: 'tarjanForDiGraphArray' " +
            "should return " +
            "all `SCCs` sizes in 'DiGraphArray'"
        ) {
            val sourceSize: Int = 5105043
            val expectedNodesSize: Int = 875714
            val expectedArcsSize: Int = 5105043
            /*all 'nodes', but only few / some 'arcs'*/
            /*at least as big as `mockUp(14)`*/
            val inputTakeNumber: Int =
            //250000 +
            //250000 +
              125000 +
                125000 +
                62500 +
                62500 +
                31250 +
                31250 +
                //31250 +
                15625 +
                15625 +
                7812 +
                7812 +
                7812 +
                7812 +
                //7812 +
                //3906 +
                //3906 +
                //3906 +
                3906 +
                1953 +
                1953 +
                976 +
                //976 +
                488 +
                //488 +
                //244 +
                122 +
                122 +
                //122
                61 +
                //61 +
                //30 +
                //15 +
                7
            //500000
            val expectedNodesInSCC: Int = 3
            val expectedSCCsInDiGraph: Int = 5 //4
    val expectedSize: Int = 4
            val filePath: String =
              "/media/gluk-alex/" +
                "GDI/Java/Scala/sbt/projects/" +
                "stanfordAlgorithmsDesignAndAnalysis1/" +
                "src/test/scala/" +
                "testSCC/"
            val fileName: String =
              "SCC.txt"
            //"tinyDG.txt"
            //"diGraphWith4SCCs"
            val actualFileContent: Iterator[String] =
            //Iterator.empty
              readFromFile(
                            fileName = fileName,
                            filePath = filePath
                          )
              /*reduce / control input size*/
              .take(inputTakeNumber)
            /*val firstFiveStrIter: Iterator[String] =
              readFromFile(
                            fileName = fileName,
                            filePath = filePath
                          )
              .take(25)*/
            val nodesInGraph: Int =
            //  actualFileContent.next().toInt
              875714
            val edgesInGraph: Int =
            //  actualFileContent.next().toInt
              5105043
            /*println(
                     s"\ntotal 'nodesInGraph`:$nodesInGraph" +
                       s"\ntotal 'edgesInGraph`:$edgesInGraph" //+
                     //s"\ntotal 'adjusted` for node '1':" +
                     //List(1,2,5,6,7,3,8,4,47646,47647,13019,47648,47649,
                     // 47650,7700,47651,47652,511596,1,9,10,11,12,13,14).size +
                     //s"\n'firstFiveStrIter`:"+
                     //firstFiveStrIter.mkString(",")
                   )*/

            /*only '5' max matter*/
            val takeNumber: Int = 5 //15
    val nodesLimit: Int = Int.MinValue

            /*val initialDiGraphArray: DiGraphArray =
              new DiGraphArray(minKeyVal = 1,
                               maxKeyVal =
                                 nodesInGraph)*/
            /*println(
                     s"\n'initialDiGraphArray.nodesSize` is:" +
                       initialDiGraphArray.nodesSize +
                       s"\n'initialDiGraphArray.nodes.length` is:" +
                       initialDiGraphArray.nodes.length +
                       s"\n'initialDiGraphArray.nodes.tail.head' is:" +
                       initialDiGraphArray.nodes.tail.head
                   )
                  initialDiGraphArray
                  .addEdge(
                      arcTail = 0,
                      arcHead = 1
                          )
                  println(
                             s"\n'initialDiGraphArray.nodes.head' is:" +
                             initialDiGraphArray.nodes.head +
                  s"\n'initialDiGraphArray.nodes.tail.head' is:" +
                             initialDiGraphArray.nodes.tail.head
                         )*/

            val diGraphArray: DiGraphArray =
              fillDiGraphArrayWithArcs(
                                        fileContentIter =
                                          actualFileContent,
                                        result =
                                          //initialDiGraphArray,
                                          /*new DiGraphArray(minKeyVal = 1,
                                                           maxKeyVal =
                                                             nodesInGraph),*/
                                          DiGraphArray
                                          .init(
                                              minKeyVal = 1,
                                              maxKeyVal =
                                                nodesInGraph),
                                        pattern =
                                          """\d+""".r
                                      )
            /*println(
                     s"\n'arcs` are extracted from file" +
                       //s"\n'diGraphArray.nodes.head' is:" +
                       //diGraphArray.nodes.head +
                       s"\n'diGraphArray.nodes.tail.head' is:" +
                       diGraphArray.nodes.tail.head
                   )*/

            val allSCCs:
            //Iterable[List[Int]] =
            //List[Int] =
            Stream[Int] =
              tarjanForDiGraphArray(diGraphArray.nodes)
              .view
              .sorted(Ordering[Int].reverse)
              .take(takeNumber)
              //.toList
              .toStream
            println(
                     s"\n'allSCCs.size' is:" +
                       allSCCs.size +
                       /*s"\n'mapWithAdjacencyList.head' is:" +
                         directedGraphDynamic
                           .nodesWithAdjusted
                         .head +
                         s"\n'mapWithAdjacencyList.tail.head' is ${
                           directedGraphDynamic
                           .nodesWithAdjusted
                           .tail.head
                         }" +*/
                       //s"\n'nodesWithAdjusted` is:" +
                       s"\n`inputTakeNumber` is $inputTakeNumber" +
                       s"\nfirst $takeNumber elements in " +
                       s"'nodesWithAdjusted`" +
                       s" are:" +
                       s"\n${
                         allSCCs
                         .take(takeNumber)
                         .mkString(",")
                       }"
                   )

            assume(
                    //true == true,
                    allSCCs
                    .nonEmpty &&
                      allSCCs
                      .size == expectedSCCsInDiGraph,
                    s"\n'allSCCs' must be 'nonEmpty' " +
                      s"& equal to 'expectedSCCsInDiGraph'"
                  )
          }
  ignore(
          "70: 'traverse' " +
            "should return " +
            "all reachable from specified `nodes` in 'DiGraphSetsMap'"
        ) {
            val startFromNode: Int =
            //8
              7
            val sourceSize: Int = 5105043
            val expectedNodesSize: Int = 875714
            val expectedArcsSize: Int = 5105043
            /*all 'nodes', but only few / some 'arcs'*/
            /*at least as big as `mockUp(14)`*/
            val inputTakeNumber: Int =
            //250000 +
            //250000 +
              125000 +
                125000 +
                62500 +
                62500 +
                31250 +
                31250 +
                //31250 +
                15625 +
                15625 +
                7812 +
                7812 +
                7812 +
                7812 +
                //7812 +
                //3906 +
                //3906 +
                //3906 +
                3906 +
                1953 +
                1953 +
                976 +
                //976 +
                488 +
                //488 +
                //244 +
                122 +
                122 +
                //122
                61 +
                //61 +
                //30 +
                //15 +
                7
            //500000
            val expectedNodesInSCC: Int = 3
            val expectedSCCsInDiGraph: Int = 5 //4
    val expectedSize: Int = 4
            val filePath: String =
              "/media/gluk-alex/" +
                "GDI/Java/Scala/sbt/projects/" +
                "stanfordAlgorithmsDesignAndAnalysis1/" +
                "src/test/scala/" +
                "testSCC/"
            val fileName: String =
            //"SCC.txt"
            //"tinyDG.txt"
              "diGraphWith4SCCs"
            /*There is
            only one standard operation
            which allows to
            re-use the same `iterator`:
            The call
             */
            //val (it1, it2) = it.duplicate
            /*gives you
            two `iterators`
            which each return
            exactly the same `elements` as
            the iterator 'it'.
            The two `iterators` work independently;
            advancing one does not affect the other.
            By contrast
            the original iterator 'it' is
            advanced to its `end`
            by `duplicate` and
            is thus rendered `unusable`.
             */
            val (actualFileContent, fileContentIter):
            (Iterator[String], Iterator[String]) =
            //val actualFileContent: Iterator[String] =
            //Iterator.empty
              readFromFile(
                            fileName = fileName,
                            filePath = filePath
                          )
              /*reduce / control input size*/
              .take(inputTakeNumber)
              .duplicate
            /*val firstFiveStrIter: Iterator[String] =
              readFromFile(
                            fileName = fileName,
                            filePath = filePath
                          )
              .take(25)*/
            /*
            Calling 'head' on a `buffered` `iterator`
            will return
            its first `element` but
            will not `advance` the `iterator`.
             */
            val bit: BufferedIterator[String] =
              fileContentIter
              .buffered
            def skipEmptyWords(it: BufferedIterator[String]) =
              while (it.head.isEmpty) {it.next()}

            val nodesInGraph: Int =
              actualFileContent.next().toInt
            //875714
            val edgesInGraph: Int =
              actualFileContent.next().toInt
            //5105043
            println(
                     s"\ntotal 'nodesInGraph`:$nodesInGraph" +
                       s"\ntotal 'edgesInGraph`:$edgesInGraph" //+
                     //s"\ntotal 'adjusted` for node '1':" +
                     //List(1,2,5,6,7,3,8,4,47646,47647,13019,47648,47649,
                     // 47650,7700,47651,47652,511596,1,9,10,11,12,13,14).size +
                     //s"\n'firstFiveStrIter`:"+
                     //firstFiveStrIter.mkString(",")
                   )

            /*only '5' max matter*/
            val takeNumber: Int = 5 //15
    val nodesLimit: Int = Int.MinValue

            /*val initialDiGraphArray: DiGraphArray =
              new DiGraphArray(minKeyVal = 1,
                               maxKeyVal =
                                 nodesInGraph)*/
            /*println(
                     s"\n'initialDiGraphArray.nodesSize` is:" +
                       initialDiGraphArray.nodesSize +
                       s"\n'initialDiGraphArray.nodes.length` is:" +
                       initialDiGraphArray.nodes.length +
                       s"\n'initialDiGraphArray.nodes.tail.head' is:" +
                       initialDiGraphArray.nodes.tail.head
                   )
                  initialDiGraphArray
                  .addEdge(
                      arcTail = 0,
                      arcHead = 1
                          )
                  println(
                             s"\n'initialDiGraphArray.nodes.head' is:" +
                             initialDiGraphArray.nodes.head +
                  s"\n'initialDiGraphArray.nodes.tail.head' is:" +
                             initialDiGraphArray.nodes.tail.head
                         )*/

            val diGraphSetsMap: DiGraphSetsMap =
              fillSetsMapFromArcs(
                                   fileContentIter =
                                     actualFileContent,
                                   pattern =
                                     """\d+""".r
                                 )
            val diGraphNodes: Map[Int, Set[Int]] =
              diGraphSetsMap.nodesWithAdjusted
            println(
                     s"\n'arcs` are extracted from file" +
                       s"\n'diGraphNodes.head' is:" +
                       diGraphNodes.headOption //+
                     //s"\n'diGraphSetsMap.nodes.tail.head' is:" +
                     //diGraphNodes.tail.head
                   )

            val allReachableNodes:
            //Iterable[List[Int]] =
            List[Int] =
            //Stream[Int] =
              traverse(
                        graph = diGraphNodes,
                        start = startFromNode)
            //.view
            //.sorted(Ordering[Int].reverse)
            //.take(takeNumber)
            //.toList
            //.toStream
            println(
                     s"\n'allSCCs.size' is:" +
                       allReachableNodes.size +
                       /*s"\n'mapWithAdjacencyList.head' is:" +
                         directedGraphDynamic
                           .nodesWithAdjusted
                         .head +
                         s"\n'mapWithAdjacencyList.tail.head' is ${
                           directedGraphDynamic
                           .nodesWithAdjusted
                           .tail.head
                         }" +*/
                       //s"\n'nodesWithAdjusted` is:" +
                       s"\n`inputTakeNumber` is $inputTakeNumber" +
                       s"\nfirst $takeNumber elements in " +
                       s"'nodesWithAdjusted`" +
                       s" are:" +
                       s"\n${
                         allReachableNodes
                         .take(takeNumber)
                         .mkString(",")
                       }"
                   )

            assume(
                    //true == true,
                    allReachableNodes
                    .nonEmpty &&
                      allReachableNodes
                      .size == nodesInGraph,
                    s"\n'allReachableNodes' must be 'nonEmpty' " +
                      s"& equal to 'nodesInGraph'"
                  )
          }
  ignore(
          "71: 'tarjanOnArray' " +
            "should return " +
            "all SCCs sizes in 'Array[NodeFieldsArray]' graph"
        ) {
            val startFromNode: Int =
            //8
              7
            val sourceSize: Int = 5105043
            val expectedNodesSize: Int = 875714
            val expectedArcsSize: Int = 5105043
            /*all 'nodes', but only few / some 'arcs'*/
            /*at least as big as `mockUp(14)`*/
            val inputTakeNumber: Int =
              expectedArcsSize
            //250000 +
            //250000 +
            /*125000 +
              125000 +
              62500 +
              62500 +
              31250 +
              31250 +
              //31250 +
              15625 +
              15625 +
              7812 +
              7812 +
              7812 +
              7812 +
              //7812 +
              //3906 +
              //3906 +
              //3906 +
              3906 +
              1953 +
              1953 +
              976 +
              //976 +
              488 +
              //488 +
              //244 +
              122 +
              122 +
              //122
              61 +
              //61 +
              //30 +
              //15 +
              7*/
            //500000
            val expectedNodesInSCC: Int = 3
            val expectedSCCsInDiGraph: Int = 5 //4
    val expectedSize: Int = 4
            val filePath: String =
              "/media/gluk-alex/" +
                "GDI/Java/Scala/sbt/projects/" +
                "stanfordAlgorithmsDesignAndAnalysis1/" +
                "src/test/scala/" +
                "testSCC/"
            val fileName: String =
              "SCC.txt"
            //"tinyDG.txt"
            //"tinyDAG.txt"
            //"diGraphWith4SCCs"
            /*There is
            only one standard operation
            which allows to
            re-use the same `iterator`:
            The call
             */
            //val (it1, it2) = it.duplicate
            /*gives you
            two `iterators`
            which each return
            exactly the same `elements` as
            the iterator 'it'.
            The two `iterators` work independently;
            advancing one does not affect the other.
            By contrast
            the original iterator 'it' is
            advanced to its `end`
            by `duplicate` and
            is thus rendered `unusable`.
             */
            val (actualFileContent, fileContentIter):
            (Iterator[String], Iterator[String]) =
            //val actualFileContent: Iterator[String] =
            //Iterator.empty
              readFromFile(
                            fileName = fileName,
                            filePath = filePath
                          )
              /*reduce / control input size*/
              //.take(inputTakeNumber)
              .duplicate
            /*val firstFiveStrIter: Iterator[String] =
              readFromFile(
                            fileName = fileName,
                            filePath = filePath
                          )
              .take(25)*/
            if (inputTakeNumber >= expectedArcsSize) {
              println(s"fetching all($expectedArcsSize) available arcs")
            }
            /*
            Calling 'head' on a `buffered` `iterator`
            will return
            its first `element` but
            will not `advance` the `iterator`.
             */
            val bit: BufferedIterator[String] =
              fileContentIter
              .buffered
            def skipEmptyWords(it: BufferedIterator[String]) =
              while (it.head.isEmpty) {it.next()}

            val nodesInGraph: Int =
            //actualFileContent.next().toInt
              875714
            val edgesInGraph: Int =
            //actualFileContent.next().toInt
              5105043
            println(
                     s"\ntotal 'nodesInGraph`:$nodesInGraph" +
                       s"\ntotal 'edgesInGraph`:$edgesInGraph" //+
                     //s"\ntotal 'adjusted` for node '1':" +
                     //List(1,2,5,6,7,3,8,4,47646,47647,13019,47648,47649,
                     // 47650,7700,47651,47652,511596,1,9,10,11,12,13,14).size +
                     //s"\n'firstFiveStrIter`:"+
                     //firstFiveStrIter.mkString(",")
                   )

            /*only '5' max matter*/
            val takeNumber: Int = 5 //15
    val nodesLimit: Int = Int.MinValue

            /*val initialDiGraphArray: DiGraphArray =
              new DiGraphArray(minKeyVal = 1,
                               maxKeyVal =
                                 nodesInGraph)*/
            /*println(
                     s"\n'initialDiGraphArray.nodesSize` is:" +
                       initialDiGraphArray.nodesSize +
                       s"\n'initialDiGraphArray.nodes.length` is:" +
                       initialDiGraphArray.nodes.length +
                       s"\n'initialDiGraphArray.nodes.tail.head' is:" +
                       initialDiGraphArray.nodes.tail.head
                   )
                  initialDiGraphArray
                  .addEdge(
                      arcTail = 0,
                      arcHead = 1
                          )
                  println(
                             s"\n'initialDiGraphArray.nodes.head' is:" +
                             initialDiGraphArray.nodes.head +
                  s"\n'initialDiGraphArray.nodes.tail.head' is:" +
                             initialDiGraphArray.nodes.tail.head
                         )*/

            val diGraphArray: DiGraphArray =
            /*fail here ? no way !*/
              fillDiGraphArrayWithArcs(
                                        fileContentIter =
                                          actualFileContent,
                                        result =
                                          //initialDiGraphArray,
                                          /*new DiGraphArray(minKeyVal = 1,
                                                           maxKeyVal =
                                                             nodesInGraph),*/
                                          DiGraphArray
                                          .init(
                                              minKeyVal = 1,
                                              maxKeyVal =
                                                nodesInGraph),
                                        pattern =
                                          """\d+""".r
                                      )
            println(
                     s"\n'arcs` are extracted from file" /*+
                     s"\n'diGraphArray.nodes.head' is:" +
                     diGraphArray.nodes.head +
                     s"\n'diGraphArray.nodes.tail.head' is:" +
                     diGraphArray.nodes.tail.head*/
                   )

            val allSCCs:
            //Iterable[List[Int]] =
            //List[Int] =
            Stream[Int] =
              Stream.empty
            /*tarjanOnArray(diGraphArray.nodes)
            .view
            .sorted(Ordering[Int].reverse)
            .take(takeNumber)
            //.toList
            .toStream*/
            println(
                     s"\n'allSCCs.size' is:" +
                       allSCCs.size +
                       /*s"\n'mapWithAdjacencyList.head' is:" +
                         directedGraphDynamic
                           .nodesWithAdjusted
                         .head +
                         s"\n'mapWithAdjacencyList.tail.head' is ${
                           directedGraphDynamic
                           .nodesWithAdjusted
                           .tail.head
                         }" +*/
                       //s"\n'nodesWithAdjusted` is:" +
                       s"\n`inputTakeNumber` is $inputTakeNumber" +
                       s"\nfirst $takeNumber elements in " +
                       s"'nodesWithAdjusted`" +
                       s" are:" +
                       s"\n${
                         allSCCs
                         .take(takeNumber)
                         .mkString(",")
                       }"
                   )

            assume(
                    //true == true,
                    allSCCs
                    .nonEmpty &&
                      allSCCs
                      .sum == nodesInGraph,
                    //.size == expectedSCCsInDiGraph,
                    s"\n'allSCCs' must be 'nonEmpty' " +
                      s"& equal to 'nodesInGraph'"
                  )
          }

  ignore(
          "72: 'postOrderOnArray' " +
            "should return " +
            "right depth-first traversal post-order for graph"
        ) {
            val startFromNode: Int =
            //8
              7
            val sourceSize: Int = 5105043
            val expectedNodesSize: Int = 875714
            val expectedArcsSize: Int = 5105043
            /*all 'nodes', but only few / some 'arcs'*/
            /*at least as big as `mockUp(14)`*/
            val inputTakeNumber: Int =
            //250000 +
            //250000 +
              125000 +
                125000 +
                62500 +
                62500 +
                31250 +
                31250 +
                //31250 +
                15625 +
                15625 +
                7812 +
                7812 +
                7812 +
                7812 +
                //7812 +
                //3906 +
                //3906 +
                //3906 +
                3906 +
                1953 +
                1953 +
                976 +
                //976 +
                488 +
                //488 +
                //244 +
                122 +
                122 +
                //122
                61 +
                //61 +
                //30 +
                //15 +
                7
            //500000
            val expectedNodesInSCC: Int = 3
            val expectedSCCsInDiGraph: Int = 5 //4
    val expectedSize: Int = 4
            val filePath: String =
              "/media/gluk-alex/" +
                "GDI/Java/Scala/sbt/projects/" +
                "stanfordAlgorithmsDesignAndAnalysis1/" +
                "src/test/scala/" +
                "testSCC/"
            val fileName: String =
            //"SCC.txt"
              "tinyDG.txt"
            //"tinyDAG.txt"
            //"diGraphWith4SCCs"
            /*There is
            only one standard operation
            which allows to
            re-use the same `iterator`:
            The call
             */
            //val (it1, it2) = it.duplicate
            /*gives you
            two `iterators`
            which each return
            exactly the same `elements` as
            the iterator 'it'.
            The two `iterators` work independently;
            advancing one does not affect the other.
            By contrast
            the original iterator 'it' is
            advanced to its `end`
            by `duplicate` and
            is thus rendered `unusable`.
             */
            val (actualFileContent, fileContentIter):
            (Iterator[String], Iterator[String]) =
            //val actualFileContent: Iterator[String] =
            //Iterator.empty
              readFromFile(
                            fileName = fileName,
                            filePath = filePath
                          )
              /*reduce / control input size*/
              //.take(inputTakeNumber)
              .duplicate
            /*val firstFiveStrIter: Iterator[String] =
              readFromFile(
                            fileName = fileName,
                            filePath = filePath
                          )
              .take(25)*/
            if (inputTakeNumber >= expectedArcsSize) {
              println(s"fetching all($expectedArcsSize) available arcs")
            }
            /*
            Calling 'head' on a `buffered` `iterator`
            will return
            its first `element` but
            will not `advance` the `iterator`.
             */
            val bit: BufferedIterator[String] =
              fileContentIter
              .buffered
            def skipEmptyWords(it: BufferedIterator[String]) =
              while (it.head.isEmpty) {it.next()}

            val nodesInGraph: Int =
              actualFileContent.next().toInt
            //875714
            val edgesInGraph: Int =
              actualFileContent.next().toInt
            //5105043
            println(
                     s"\ntotal 'nodesInGraph`:$nodesInGraph" +
                       s"\ntotal 'edgesInGraph`:$edgesInGraph" //+
                     //s"\ntotal 'adjusted` for node '1':" +
                     //List(1,2,5,6,7,3,8,4,47646,47647,13019,47648,47649,
                     // 47650,7700,47651,47652,511596,1,9,10,11,12,13,14).size +
                     //s"\n'firstFiveStrIter`:"+
                     //firstFiveStrIter.mkString(",")
                   )

            /*only '5' max matter*/
            val takeNumber: Int = 5 //15
    val nodesLimit: Int = Int.MinValue

            val diGraphArray: DiGraphArray =
              fillDiGraphArrayWithArcs(
                                        fileContentIter =
                                          actualFileContent,
                                        result =
                                          //initialDiGraphArray,
                                          /*new DiGraphArray(minKeyVal = 1,
                                                           maxKeyVal =
                                                             nodesInGraph),*/
                                          DiGraphArray
                                          .init(
                                              minKeyVal = 1,
                                              maxKeyVal =
                                                nodesInGraph),
                                        pattern =
                                          """\d+""".r
                                      )
            println(
                     s"\n'arcs` are extracted from file" +
                       s"\ninitial 'diGraphArray` is:\n" +
                       diGraphArray.nodes
                       .take(15)
                       .map(n =>
                              n.nodeKey + "" +
                                n.adjustedNodes.mkString("{", ",", "}"))
                       .mkString(",") //+
                     /*s"\n'diGraphArray.nodes.head' is:" +
                     diGraphArray.nodes.head +
                     s"\n'diGraphArray.nodes.tail.head' is:" +
                     diGraphArray.nodes.tail.head*/
                   )
            val diGraphArrayReversed: DiGraphArray =
              fillDiGraphArrayWithArcs(
                                        fileContentIter =
                                          fileContentIter.drop(2),
                                        result =
                                          //initialDiGraphArray,
                                          /*new DiGraphArray(minKeyVal = 1,
                                                           maxKeyVal =
                                                             nodesInGraph),*/
                                          DiGraphArray
                                          .init(
                                              minKeyVal = 1,
                                              maxKeyVal =
                                                nodesInGraph),
                                        pattern =
                                          """\d+""".r,
                                        nonReversedArcs = false
                                      )
            println(
                     s"\n'arcs` are extracted from file" +
                       s"\n'diGraphArrayReversed` is:\n" +
                       diGraphArrayReversed.nodes
                       .take(15)
                       .map(n =>
                              n.nodeKey + "" +
                                n.adjustedNodes.mkString("{", ",", "}"))
                       .mkString(",") // +
                     /*s"\n'diGraphArray.nodes.head' is:" +
                     diGraphArray.nodes.head +
                     s"\n'diGraphArray.nodes.tail.head' is:" +
                     diGraphArray.nodes.tail.head*/
                   )

            //1,3,2,4,5,0,11,9,12,10,8,6,7
            val graphPostOrder:
            //Iterable[List[Int]] =
            //List[Int] =
            Stream[Int] =
              postOrderOnArray(diGraphArray.nodes /*,Some(0)*/)
            /*.view
            .sorted(Ordering[Int].reverse)
            .take(takeNumber)
            //.toList
            .toStream*/
            println(
                     s"\n'graphPostOrder.size' is:" +
                       graphPostOrder.size +
                       s"\n'graphPostOrder' is:\n${
                         graphPostOrder
                         .take(15)
                         .mkString(",")
                       }"
                   )
            //8,7,6,10,12,9,11,3,5,4,2,0,1
            val reversedGraphPostOrder:
            //Iterable[List[Int]] =
            //List[Int] =
            Stream[Int] =
              postOrderOnArray(diGraphArrayReversed.nodes /*,Some(0)*/)
            /*.view
            .sorted(Ordering[Int].reverse)
            .take(takeNumber)
            //.toList
            .toStream*/
            println(
                     //s"\n'graphPostOrder.size' is:" +
                     //graphPostOrder.size +
                     s"\n'reversedGraphPostOrder' is:\n${
                       reversedGraphPostOrder
                       .take(15)
                       .mkString(",")
                     }"
                   )

            assume(
                    //true == true,
                    graphPostOrder
                    .nonEmpty &&
                      graphPostOrder
                      .size == nodesInGraph,
                    s"\n'allSCCs' must be 'nonEmpty' " +
                      s"& equal to 'nodesInGraph'"
                  )
          }
  /*TODO test & beBug*/
  ignore(
          "73: 'postOrderOnArray' then 'iterativeDFS_OnArray'" +
            "should return " +
            "all SCCs in graph"
        ) {
            val startFromNode: Int =
            //8
              7
            val sourceSize: Int = 5105043
            val expectedNodesSize: Int = 875714
            val expectedArcsSize: Int = 5105043
            /*all 'nodes', but only few / some 'arcs'*/
            /*at least as big as `mockUp(14)`*/
            val inputTakeNumber: Int =
              expectedArcsSize
            //250000 +
            /*125000 +
              125000 +
              62500 +
              62500 +
              31250 +
              31250 +
              //31250 +
              15625 +
              15625 +
              7812 +
              7812 +
              7812 +
              7812 +
              //7812 +
              //3906 +
              //3906 +
              //3906 +
              3906 +
              1953 +
              1953 +
              976 +
              //976 +
              488 +
              //488 +
              //244 +
              122 +
              122 +
              //122
              61 +
              //61 +
              //30 +
              //15 +
              7*/
            //500000
            val expectedNodesInSCC: Int = 3
            val expectedSCCsInDiGraph: Int = 5
            val expectedSize: Int = 4
            val filePath: String =
              "/media/gluk-alex/" +
                "GDI/Java/Scala/sbt/projects/" +
                "stanfordAlgorithmsDesignAndAnalysis1/" +
                "src/test/scala/" +
                "testSCC/"
            val fileName: String =
            //"SCC.txt"
            //"tinyDG.txt"
            //"tinyDAG.txt"
              "diGraphWith4SCCs"
            /*There is
            only one standard operation
            which allows to
            re-use the same `iterator`:
            The call
             */
            //val (it1, it2) = it.duplicate
            /*gives you
            two `iterators`
            which each return
            exactly the same `elements` as
            the iterator 'it'.
            The two `iterators` work independently;
            advancing one does not affect the other.
            By contrast
            the original iterator 'it' is
            advanced to its `end`
            by `duplicate` and
            is thus rendered `unusable`.
             */
            val (actualFileContent, fileContentIter):
            (Iterator[String], Iterator[String]) =
            //val actualFileContent: Iterator[String] =
            //Iterator.empty
              readFromFile(
                            fileName = fileName,
                            filePath = filePath
                          )
              /*reduce / control input size*/
              .take(inputTakeNumber)
              .duplicate
            /*val firstFiveStrIter: Iterator[String] =
              readFromFile(
                            fileName = fileName,
                            filePath = filePath
                          )
              .take(25)*/

            /*
            Calling 'head' on a `buffered` `iterator`
            will return
            its first `element` but
            will not `advance` the `iterator`.
             */
            /*val bit: BufferedIterator[String] =
              fileContentIter
              .buffered
            def skipEmptyWords(it: BufferedIterator[String]) =
              while (it.head.isEmpty) {it.next()}*/

            val nodesInGraph: Int =
              actualFileContent.next().toInt
            //875714
            val edgesInGraph: Int =
              actualFileContent.next().toInt
            //5105043
            println(
                     s"\ntotal 'nodesInGraph`:$nodesInGraph" +
                       s"\ntotal 'edgesInGraph`:$edgesInGraph" //+
                     //s"\ntotal 'adjusted` for node '1':" +
                     //List(1,2,5,6,7,3,8,4,47646,47647,13019,47648,47649,
                     // 47650,7700,47651,47652,511596,1,9,10,11,12,13,14).size +
                     //s"\n'firstFiveStrIter`:"+
                     //firstFiveStrIter.mkString(",")
                   )

            /*only '5' max matter*/
            val takeNumber: Int = 5
            val nodesLimit: Int = Int.MinValue

            val diGraphArray: DiGraphArray =
              fillDiGraphArrayWithArcs(
                                        fileContentIter =
                                          actualFileContent,
                                        result =
                                          //initialDiGraphArray,
                                          /*new DiGraphArray(minKeyVal = 1,
                                                           maxKeyVal =
                                                             nodesInGraph),*/
                                          DiGraphArray
                                          .init(
                                              minKeyVal = 1,
                                              maxKeyVal =
                                                nodesInGraph),
                                        pattern =
                                          """\d+""".r
                                      )
            println(
                     s"'arcs` are extracted from file" /*+
                   s"\ninitial 'diGraphArray` is:\n" +
                     diGraphArray.nodes
                     .take(15)
                     .map(n =>
                            n.nodeKey + "" +
                              n.adjustedNodes.mkString("{",",","}"))
                     .mkString(",")*/
                     //+
                     /*s"\n'diGraphArray.nodes.head' is:" +
                     diGraphArray.nodes.head +
                     s"\n'diGraphArray.nodes.tail.head' is:" +
                     diGraphArray.nodes.tail.head*/
                   )
            val diGraphArrayReversed: DiGraphArray =
              fillDiGraphArrayWithArcs(
                                        fileContentIter =
                                          fileContentIter.drop(2),
                                        result =
                                          //initialDiGraphArray,
                                          /*new DiGraphArray(minKeyVal = 1,
                                                           maxKeyVal =
                                                             nodesInGraph),*/
                                          DiGraphArray
                                          .init(
                                              minKeyVal = 1,
                                              maxKeyVal =
                                                nodesInGraph),
                                        pattern =
                                          """\d+""".r,
                                        nonReversedArcs = false
                                      )
            println(
                     s"Reversed 'arcs` are extracted from file" /*+
                   s"\n'diGraphArrayReversed` is:\n" +
                     diGraphArrayReversed.nodes
                     .take(15)
                       .map(n =>
                              n.nodeKey + "" +
                                n.adjustedNodes.mkString("{",",","}"))
                     .mkString(",")*/
                     // +
                     /*s"\n'diGraphArray.nodes.head' is:" +
                     diGraphArray.nodes.head +
                     s"\n'diGraphArray.nodes.tail.head' is:" +
                     diGraphArray.nodes.tail.head*/
                   )

            //1,3,2,4,5,0,11,9,12,10,8,6,7
            /*val graphPostOrder:
            //Iterable[List[Int]] =
            //List[Int] =
            Stream[Int] =
              postOrderOnArray(diGraphArray.nodes /*,Some(0)*/)
            /*.view
            .sorted(Ordering[Int].reverse)
            .take(takeNumber)
            //.toList
            .toStream*/
            println(
                     s"\n'graphPostOrder.size' is:" +
                       graphPostOrder.size +
                       s"\n'graphPostOrder' is:\n${
                         graphPostOrder
                         .take(15)
                         .mkString(",")
                       }"
                   )*/
            //8,7,6,10,12,9,11,3,5,4,2,0,1
            val reversedGraphPostOrder:
            //Iterable[List[Int]] =
            //List[Int] =
            Stream[Int] =
              postOrderOnArray(
                                //diGraphArrayReversed
                                diGraphArray
                                .nodes /*,
                                Some(0)*/)
            /*.view
            .sorted(Ordering[Int].reverse)
            .take(takeNumber)
            //.toList
            .toStream*/
            println(
                     //s"\n'graphPostOrder.size' is:" +
                     //graphPostOrder.size +
                     s"'reversedGraphPostOrder' is:\n${
                       reversedGraphPostOrder
                       .take(15)
                       .mkString(",")
                     }"
                   )

            println(
                     s"starting collecting 'allSCCs' ...")
            val allSCCs:
            //Iterable[List[Int]] =
            Iterable[List[Int]] =
            //Stream[Stream[Int]] =
              iterativeDFS_OnArray(
                                    adjacencyList =
                                      diGraphArrayReversed.nodes,
                                    //diGraphArray.nodes,
                                    postOrderNodesStream =
                                      reversedGraphPostOrder
                                  )
              .view
              //.sorted(Ordering[Int].reverse)
              //.take(takeNumber)
              .toList
            //.toStream
            println(
                     s"'allSCCs.size' is:" +
                       allSCCs.size +
                       /*s"\n'mapWithAdjacencyList.head' is:" +
                         directedGraphDynamic
                           .nodesWithAdjusted
                         .head +
                         s"\n'mapWithAdjacencyList.tail.head' is ${
                           directedGraphDynamic
                           .nodesWithAdjusted
                           .tail.head
                         }" +*/
                       //s"\n'nodesWithAdjusted` is:" +
                       s"\n`inputTakeNumber` is $inputTakeNumber" +
                       s"\nfirst $takeNumber elements in " +
                       s"'allSCCs`" +
                       s" are:" +
                       s"\n${
                         allSCCs
                         .take(takeNumber)
                         .map(n => n.mkString("{", ",", "}"))
                         .mkString(",")
                       }"
                   )

            assume(
                    //true == true,
                    allSCCs
                    .nonEmpty &&
                      allSCCs
                      .flatMap(_.map(_ => 1))
                      .sum == nodesInGraph,
                    //.size == expectedSCCsInDiGraph,
                    s"\n'allSCCs' must be 'nonEmpty' " +
                      s"& equal to 'nodesInGraph'"
                  )
          }
  /*too slow on "SCC.txt", stack on '2515000' arcs*/
  ignore(
          "74: 'fillDiGraphArrayWithArcs'" +
            "should " +
            "read all arcs from input" +
            " and add them in graph"
        ) {
            val startFromNode: Int =
            //8
              7
            val sourceSize: Int = 5105043
            val expectedNodesSize: Int = 875714
            val expectedArcsSize: Int = 5105043
            val inputTakeNumber: Int =
              5105043
            //500000
            //250000 +
            //125000 +
            //62500 +
            //31250 +
            //15625 +
            //7812 +
            //3906 +
            //1953 +
            //976 +
            //488 +
            //244 +
            //122
            //61 +
            //30 +
            //15 +
            //7

            val expectedNodesInSCC: Int = 3
            val expectedSCCsInDiGraph: Int = 5
            val expectedSize: Int = 4
            val filePath: String =
              "/home/gluk-alex/Documents/sbt_projects/"
            /*"/media/gluk-alex/" +
              "GDI/Java/Scala/sbt/projects/" +
              "stanfordAlgorithmsDesignAndAnalysis1/" +
              "src/test/scala/" +
              "testSCC/"*/
            val fileName: String =
              "SCC.txt"
            //"tinyDG.txt"
            //"tinyDAG.txt"
            //"diGraphWith4SCCs"
            val (actualFileContent, fileContentIter):
            (Iterator[String], Iterator[String]) =
            //Iterator.empty
              readFromFile(
                            fileName = fileName,
                            filePath = filePath
                          )
              /*reduce / control input size*/
              //.take(inputTakeNumber)
              .duplicate

            /*
            Calling 'head' on a `buffered` `iterator`
            will return
            its first `element` but
            will not `advance` the `iterator`.
             */
            /*val bit: BufferedIterator[String] =
              fileContentIter
              .buffered
            def skipEmptyWords(it: BufferedIterator[String]) =
              while (it.head.isEmpty) {it.next()}*/

            val nodesInGraph: Int =
            //actualFileContent.next().toInt
              875714
            val edgesInGraph: Int =
            //actualFileContent.next().toInt
              5105043
            println(
                     s"\ntotal 'nodesInGraph`:$nodesInGraph" +
                       s"\ntotal 'edgesInGraph`:$edgesInGraph" //+
                     //s"\ntotal 'adjusted` for node '1':" +
                     //List(1,2,5,6,7,3,8,4,47646,47647,13019,47648,47649,
                     // 47650,7700,47651,47652,511596,1,9,10,11,12,13,14).size +
                     //s"\n'firstFiveStrIter`:"+
                     //firstFiveStrIter.mkString(",")
                   )

            /*only '5' max matter*/
            val takeNumber: Int = 5
            //val nodesLimit: Int = Int.MinValue

            val startTime: java.util.Date = Calendar.getInstance().getTime()
            lazy val timeStamp1: Long = System.currentTimeMillis
            /*Date and Time Pattern */
            val timeStampFormat = new SimpleDateFormat("HH:mm:ss.SSS")
            lazy val startStampString = timeStampFormat.format(startTime)
            println(s"'fillDiGraphArrayWithArcs' started at:" +
                      startStampString)
            println(s"timeStamp1:" + timeStamp1)

            val diGraphArrayInitial: DiGraphArray =
              DiGraphArray
              .init(
                  minKeyVal = 1,
                  maxKeyVal =
                    nodesInGraph)
            println(
                     s"'diGraphArrayInitial.nodesSize' is:" +
                       diGraphArrayInitial.nodesSize /*+
                       s"\n'diGraphArrayInitial.nodes.headOption' is:" +
                       diGraphArrayInitial.nodes.headOption +
                       s"\n'diGraphArrayInitial.nodes.lastOption' is:" +
                       diGraphArrayInitial.nodes.lastOption*/
                   )
            val diGraphArray: DiGraphArray =
              fillDiGraphArrayWithArcs(
                                        fileContentIter =
                                          actualFileContent,
                                        result =
                                          diGraphArrayInitial,
                                        pattern =
                                          """\d+""".r
                                      )

            lazy val endTime = Calendar.getInstance().getTime()
            lazy val timeStamp2: Long = System.currentTimeMillis()
            lazy val endStampString = timeStampFormat.format(endTime)
            lazy val timeDifference: Long =
              timeStamp2 - timeStamp1
            println(s"Done at:" + endStampString)
            println(s"timeStamp2:" + timeStamp2)
            println(s"time difference is:" +
                      (timeStamp2 - timeStamp1) + " Millis")
            println(s"time difference is:" +
                      convertLongToTimeString(
                                               timeNumberMillis =
                                                 timeDifference,
                                               colored = false
                                             )
                   )
            println(
                     s"'arcs` are extracted from file" +
                       s"\n'diGraphArray` biggest 'adjustedNodes.size' is:\n" +
                       diGraphArray
                       .nodes
                       //.head
                       .view
                       .sortBy(_.adjustedNodes.size)
                       .headOption /*+
                   s"\ninitial 'diGraphArray` is:\n" +
                     diGraphArray.nodes
                     .take(15)
                     .map(n =>
                            n.nodeKey + "" +
                              n.adjustedNodes.mkString("{",",","}"))
                     .mkString(",")*/
                     //+
                     /*s"\n'diGraphArray.nodes.head' is:" +
                     diGraphArray.nodes.head +
                     s"\n'diGraphArray.nodes.tail.head' is:" +
                     diGraphArray.nodes.tail.head*/
                   )

            assume(
                    //true == true,
                    diGraphArray
                    .nodes
                    .nonEmpty &&
                      diGraphArray
                      .nodesSize ==
                        nodesInGraph,
                    //fileContentIter.length,
                    //.size == expectedSCCsInDiGraph,
                    s"\n'diGraphArray.nodes' must be 'nonEmpty' " +
                      s"& equal to 'nodesInGraph'"
                  )
          }
  ignore(
          "80: 'postOrderOnMap' " +
            "should return " +
            "right depth-first traversal post-order for graph"
        ) {
            val startFromNode: Int =
            //8
              7
            val sourceSize: Int = 5105043
            val expectedNodesSize: Int = 875714
            val expectedArcsSize: Int = 5105043
            /*all 'nodes', but only few / some 'arcs'*/
            /*at least as big as `mockUp(14)`*/
            val inputTakeNumber: Int =
              expectedArcsSize
            //500000
            //250000 +
            /*125000 +
            62500 +
            //31250 +
            15625 +
            //7812 +
            //3906 +
            1953 +
            //976 +
            //488 +
            //244 +
            //122
            //61 +
            //30 +
            //15 +
            7*/

            val expectedNodesInSCC: Int = 3
            val expectedSCCsInDiGraph: Int = 5
            val expectedSize: Int = 4
            val filePath: String =
              "/media/gluk-alex/" +
                "GDI/Java/Scala/sbt/projects/" +
                "stanfordAlgorithmsDesignAndAnalysis1/" +
                "src/test/scala/" +
                "testSCC/"
            val fileName: String =
            //"SCC.txt"
              "tinyDG.txt"
            //"tinyDAG.txt"
            //"diGraphWith4SCCs"
            /*There is
            only one standard operation
            which allows to
            re-use the same `iterator`:
            The call
             */
            //val (it1, it2) = it.duplicate
            /*gives you
            two `iterators`
            which each return
            exactly the same `elements` as
            the iterator 'it'.
            The two `iterators` work independently;
            advancing one does not affect the other.
            By contrast
            the original iterator 'it' is
            advanced to its `end`
            by `duplicate` and
            is thus rendered `unusable`.
             */
            val (actualFileContent, fileContentIter):
            (Iterator[String], Iterator[String]) =
            //val actualFileContent: Iterator[String] =
            //Iterator.empty
              readFromFile(
                            fileName = fileName,
                            filePath = filePath
                          )
              /*reduce / control input size*/
              //.take(inputTakeNumber)
              .duplicate
            /*val firstFiveStrIter: Iterator[String] =
              readFromFile(
                            fileName = fileName,
                            filePath = filePath
                          )
              .take(25)*/
            if (inputTakeNumber >= expectedArcsSize) {
              println(s"fetching all($expectedArcsSize) available arcs")
            }

            /*
            Calling 'head' on a `buffered` `iterator`
            will return
            its first `element` but
            will not `advance` the `iterator`.
             */
            /*val bit: BufferedIterator[String] =
              fileContentIter
              .buffered
            def skipEmptyWords(it: BufferedIterator[String]) =
              while (it.head.isEmpty) {it.next()}*/

            val nodesInGraph: Int =
              actualFileContent.next().toInt
            //875714
            val edgesInGraph: Int =
              actualFileContent.next().toInt
            //5105043
            println(
                     s"\ntotal 'nodesInGraph`:$nodesInGraph" +
                       s"\ntotal 'edgesInGraph`:$edgesInGraph" //+
                     //s"\ntotal 'adjusted` for node '1':" +
                     //List(1,2,5,6,7,3,8,4,47646,47647,13019,47648,47649,
                     // 47650,7700,47651,47652,511596,1,9,10,11,12,13,14).size +
                     //s"\n'firstFiveStrIter`:"+
                     //firstFiveStrIter.mkString(",")
                   )

            /*only '5' max matter*/
            val takeNumber: Int = 5
            val nodesLimit: Int = Int.MinValue

            lazy val startTime: java.util.Date =
              Calendar.getInstance().getTime()
            lazy val timeStamp1: Long = System.currentTimeMillis
            /*Date and Time Pattern */
            val timeStampFormat = new SimpleDateFormat("HH:mm:ss.SSS")
            lazy val startStampString = timeStampFormat.format(startTime)
            println(s"'makeAdjacencyListMapFromArcs' started at:" +
                      startStampString)
            println(s"timeStamp1:" + timeStamp1)
            //println

            //val mapWithAdjacencyList:
            val diGraphMap:
            Map[Int, NodeMapValFieldsStatic] =
              makeAdjacencyListMapFromArcs(
                                            fileContentIter =
                                              actualFileContent
                                          )

            /*fails in IDE REPL, same as 'startTime'*/
            lazy val endTime = Calendar.getInstance().getTime()
            lazy val timeStamp2: Long = System.currentTimeMillis()
            lazy val endStampString = timeStampFormat.format(startTime)
            val timeDifference: Long =
              timeStamp2 - timeStamp1
            println(s"Done at:" + endStampString)
            println(s"timeStamp2:" + timeStamp2)
            println(s"time difference is:" +
                      (timeStamp2 - timeStamp1) + " Millis")
            println(s"time difference is:" +
                      convertLongToTimeString(
                                               timeNumberMillis =
                                                 timeDifference,
                                               colored = false
                                             )
                   )
            println(
                     s"'arcs` are extracted from file" +
                       s"\ninitial 'diGraphMap.size` is:\n" +
                       diGraphMap.size +
                       s"\n'diGraphMap` first '15' elements are:\n" +
                       diGraphMap
                       //.values
                       .view
                       .take(15)
                       .map(
                       /*(k,v): (Int,NodeMapValFieldsStatic) =>*/ { case (k,
                       v) =>
                         k + "" +
                           v.adjustedNodes.mkString("{", ",", "}")
                       }
                           )
                       .mkString(",") //+
                     /*s"\n'diGraphArray.nodes.head' is:" +
                     diGraphArray.nodes.head +
                     s"\n'diGraphArray.nodes.tail.head' is:" +
                     diGraphArray.nodes.tail.head*/
                   )

            lazy val timeStamp3: Long = System.currentTimeMillis
            println(s"Start at 'timeStamp3':" + timeStamp3)
            val diGraphMapReversed:
            Map[Int, NodeMapValFieldsStatic] =
              makeAdjacencyListMapFromArcs(
                                            fileContentIter =
                                              fileContentIter,
                                            //.drop(2)
                                            nonReversedArcs = false
                                          )
            lazy val timeStamp4: Long = System.currentTimeMillis()
            println(s"Done at 'timeStamp4':" + timeStamp2)
            println(s"time difference is:" +
                      (timeStamp4 - timeStamp3) + " Millis")
            println(s"time difference is:" +
                      convertLongToTimeString(
                                               timeNumberMillis =
                                                 timeStamp4 - timeStamp3,
                                               colored = false
                                             )
                   )
            println(
                     s"'arcs` are extracted from file" +
                       s"\n'diGraphMapReversed.size` is:\n" +
                       diGraphMapReversed.size +
                       s"\n'diGraphMapReversed` first 15 elements are:\n" +
                       diGraphMapReversed
                       .view
                       .take(15)
                       .map(
                       /*(k,v): (Int,NodeMapValFieldsStatic) => */ { case (k,
                       v) =>
                         k + "" +
                           v.adjustedNodes.mkString("{", ",", "}")
                       }
                           )
                       .mkString(",") // +
                     /*s"\n'diGraphArray.nodes.head' is:" +
                     diGraphArray.nodes.head +
                     s"\n'diGraphArray.nodes.tail.head' is:" +
                     diGraphArray.nodes.tail.head*/
                   )

            lazy val startTimeStamp5: Long = System.currentTimeMillis
            println(s"Start at 'startTimeStamp5':" + startTimeStamp5)

            //1,3,2,4,5,0,11,9,12,10,8,6,7
            val graphPostOrder:
            //Iterable[List[Int]] =
            //List[Int] =
            Stream[Int] =
              postOrderOnMap(
                              //diGraphMapReversed
                              diGraphMap
                              .view
                              //.map(elem=>(elem._1->elem._2.adjustedNodes))
                              .map({ case (key, value) => (key -> value
                                                                  .adjustedNodes)
                                   })
                              .toMap
                              /*,
                             Some(0)*/)
            /*.view
            .sorted(Ordering[Int].reverse)
            .take(takeNumber)
            //.toList
            .toStream*/

            lazy val endTimeStamp5: Long = System.currentTimeMillis()
            println(s"Done at 'endTimeStamp5':" + endTimeStamp5)
            println(
                     s"Time difference is:" +
                       (endTimeStamp5 - startTimeStamp5) + " Millis, or :" +
                       convertLongToTimeString(
                                                timeNumberMillis =
                                                  endTimeStamp5 -
                                                    startTimeStamp5,
                                                colored = false
                                              ) +
                       s" elapsed"
                   )
            println(
                     s"\n'graphPostOrder.size' is:" +
                       graphPostOrder.size +
                       s"\n'graphPostOrder' is:\n${
                         graphPostOrder
                         .take(15)
                         .mkString(",")
                       }"
                   )

            lazy val startTimeStamp6: Long = System.currentTimeMillis
            println(s"Start at 'startTimeStamp6':" + startTimeStamp6)
            //8,7,6,10,12,9,11,3,5,4,2,0,1
            val reversedGraphPostOrder:
            //Iterable[List[Int]] =
            //List[Int] =
            Stream[Int] =
              postOrderOnMap(
                              diGraphMapReversed
                              .map({ case (key, value) => (key -> value
                                                                  .adjustedNodes)
                                   })
                              .toMap
                              /*,Some(0)*/)
            /*.view
            .sorted(Ordering[Int].reverse)
            .take(takeNumber)
            //.toList
            .toStream*/

            lazy val endTimeStamp6: Long = System.currentTimeMillis()
            println(s"Done at 'endTimeStamp6':" + endTimeStamp6)
            println(
                     s"Time difference is:" +
                       (endTimeStamp6 - startTimeStamp6) + " Millis, or :" +
                       convertLongToTimeString(
                                                timeNumberMillis =
                                                  endTimeStamp6 -
                                                    startTimeStamp6,
                                                colored = false
                                              ) +
                       s" elapsed"
                   )
            println(
                     //s"\n'graphPostOrder.size' is:" +
                     //graphPostOrder.size +
                     s"\n'reversedGraphPostOrder' is:\n${
                       reversedGraphPostOrder
                       .take(15)
                       .mkString(",")
                     }"
                   )

            assume(
                    //true == true,
                    graphPostOrder
                    .nonEmpty &&
                      graphPostOrder
                      .size == nodesInGraph,
                    s"\n'allSCCs' must be 'nonEmpty' " +
                      s"& equal to 'nodesInGraph'"
                  )
          }
  ignore(
          "81: 'makeTransposeSetsMapFromSetsMap' " +
            "should return " +
            "transpose of the original graph"
        ) {
            val startFromNode: Int =
            //8
              7
            val sourceSize: Int = 5105043
            val expectedNodesSize: Int = 875714
            val expectedArcsSize: Int = 5105043
            /*all 'nodes', but only few / some 'arcs'*/
            /*at least as big as `mockUp(14)`*/
            val inputTakeNumber: Int =
              expectedArcsSize
            //500000
            //250000 +
            /*125000 +
            62500 +
            //31250 +
            15625 +
            //7812 +
            //3906 +
            1953 +
            //976 +
            //488 +
            //244 +
            //122
            //61 +
            //30 +
            //15 +
            7*/

            val expectedNodesInSCC: Int = 3
            val expectedSCCsInDiGraph: Int = 5
            val expectedSize: Int = 4
            val filePath: String =
              "/home/gluk-alex/Documents/sbt_projects/"
            /*"/media/gluk-alex/" +
              "GDI/Java/Scala/sbt/projects/" +
              "stanfordAlgorithmsDesignAndAnalysis1/" +
              "src/test/scala/" +
              "testSCC/"*/
            val fileName: String =
              "SCC.txt"
            //"tinyDG.txt"
            //"tinyDAG.txt"
            //"diGraphWith4SCCs"
            /*There is
            only one standard operation
            which allows to
            re-use the same `iterator`:
            The call
             */
            //val (it1, it2) = it.duplicate
            /*gives you
            two `iterators`
            which each return
            exactly the same `elements` as
            the iterator 'it'.
            The two `iterators` work independently;
            advancing one does not affect the other.
            By contrast
            the original iterator 'it' is
            advanced to its `end`
            by `duplicate` and
            is thus rendered `unusable`.
             */
            lazy val (actualFileContent, fileContentIter):
            (Iterator[String], Iterator[String]) =
            //val actualFileContent: Iterator[String] =
            //Iterator.empty
              readFromFile(
                            fileName = fileName,
                            filePath = filePath
                          )
              /*reduce / control input size*/
              .take(inputTakeNumber)
              .duplicate

            val nodesInGraph: Int =
              if (fileName == "SCC.txt") {
                expectedNodesSize
                //875714
              } else {
                actualFileContent.next().toInt
              }
            val edgesInGraph: Int =
              if (fileName == "SCC.txt") {
                expectedArcsSize
                //5105043
              } else {
                actualFileContent.next().toInt
              }
            println(
                     s"\ntotal 'nodesInGraph`:$nodesInGraph" +
                       s"\ntotal 'edgesInGraph`:$edgesInGraph" //+
                     //s"\ntotal 'adjusted` for node '1':" +
                     //List(1,2,5,6,7,3,8,4,47646,47647,13019,47648,47649,
                     // 47650,7700,47651,47652,511596,1,9,10,11,12,13,14).size +
                     //s"\n'firstFiveStrIter`:"+
                     //firstFiveStrIter.mkString(",")
                   )

            /*only '5' max matter*/
            val takeNumber: Int = 5
            //val nodesLimit: Int = Int.MinValue

            /*Date and Time Pattern */
            val timeStampFormat = new SimpleDateFormat("HH:mm:ss.SSS")
            lazy val startTime: java.util.Date =
              Calendar.getInstance().getTime()
            lazy val startStampString = timeStampFormat.format(startTime)
            println(s"'makeSetsMapFromArcs' for 'diGraphMap' started at:" +
                      startStampString)
            /*lazy*/ val startTimeStamp1: Long = System.currentTimeMillis
            //println(s"Start at 'startTimeStamp1':" + startTimeStamp1)

            //val mapWithAdjacencyList:
            /*lazy*/ val diGraphMap:
            Map[Int, Set[Int]] =
              makeSetsMapFromArcs(
                                   fileContentIter =
                                     actualFileContent
                                 )
            lazy val endTime = Calendar.getInstance().getTime()
            lazy val endStampString =
              timeStampFormat
              .format(endTime)
            println(s"Done at:" + endStampString)
            /*lazy*/ val endTimeStamp1: Long = System.currentTimeMillis()
            //println(s"Done at 'endTimeStamp1':" + endTimeStamp1)
            println(
                     s"Time difference is:" +
                       (endTimeStamp1 - startTimeStamp1) + " Millis, or :" +
                       convertLongToTimeString(
                                                timeNumberMillis =
                                                  endTimeStamp1 -
                                                    startTimeStamp1,
                                                colored = false
                                              ) +
                       s" elapsed"
                   )
            println(
                     s"'arcs` are extracted from file" +
                       //s"\ninitial 'diGraphMap.size` is:" +
                       //diGraphMap.size +
                       s"\n'diGraphMap` first '3' elements are:\n" +
                       diGraphMap
                       //.values
                       .view
                       .take(3)
                       .map(
                       /*(k,v): (Int,NodeMapValFieldsStatic) =>*/ { case (k,
                       v) =>
                         k + "" +
                           v.mkString("{", ",", "}")
                       }
                           )
                       .mkString(",") //+
                     /*s"\n'diGraphArray.nodes.head' is:" +
                     diGraphArray.nodes.head +
                     s"\n'diGraphArray.nodes.tail.head' is:" +
                     diGraphArray.nodes.tail.head*/
                   )

            /*lazy*/ val timeStamp3: Long = System.currentTimeMillis
            //println(s"Start at 'timeStamp3':" + timeStamp3)
            lazy val startTime2: java.util.Date =
              Calendar.getInstance().getTime()
            lazy val startStampString2 =
              timeStampFormat
              .format(startTime2)
            println(s"'makeSetsMapFromArcs' for 'diGraphMapReversed' " +
                      s"started at:" +
                      startStampString2)
            /*lazy*/ val diGraphMapReversed:
            Map[Int, Set[Int]] =
              makeSetsMapFromArcs(
                                   fileContentIter =
                                     if (fileName == "SCC.txt") {
                                       fileContentIter
                                     } else {
                                       fileContentIter
                                       .drop(2)
                                     },
                                   nonReversedArcs = false
                                 )
            lazy val endTime2 = Calendar.getInstance().getTime()
            lazy val endStampString2 = timeStampFormat.format(endTime2)
            println(s"Done at:" + endStampString2)
            /*lazy*/ val timeStamp4: Long = System.currentTimeMillis()
            //println(s"Done at 'timeStamp4':" + timeStamp2)
            /*println(s"time difference is:" +
                      (timeStamp4 - timeStamp3) + " Millis")*/
            println(s"time difference is:" +
                      convertLongToTimeString(
                                               timeNumberMillis =
                                                 timeStamp4 - timeStamp3,
                                               colored = false
                                             )
                   )
            println(
                     s"'arcs` are extracted from file" +
                       //s"\n'diGraphMapReversed.size` is:" +
                       //diGraphMapReversed.size +
                       s"\n'diGraphMapReversed` first 3 elements are:\n" +
                       diGraphMapReversed
                       .view
                       .take(3)
                       .map(
                       { case (k, v) =>
                         k + "" +
                           v.mkString("{", ",", "}")
                       }
                           )
                       .mkString(",") // +
                     /*s"\n'diGraphArray.nodes.head' is:" +
                     diGraphArray.nodes.head +
                     s"\n'diGraphArray.nodes.tail.head' is:" +
                     diGraphArray.nodes.tail.head*/
                   )

            lazy val startTime3: java.util.Date =
              Calendar.getInstance().getTime()
            lazy val startStampString3 =
              timeStampFormat
              .format(startTime3)
            println(s"'makeTransposeSetsMapFromSetsMap' started at:" +
                      startStampString3)
            /*lazy*/ val startTimeStamp3: Long = System.currentTimeMillis
            //println(s"Start at 'startTimeStamp3':" + startTimeStamp3)

            val unReversedBitSet:
            collection
            .mutable
            .BitSet =
              collection.mutable.BitSet.empty ++
                diGraphMap
                .keySet
            //.keysIterator

            //val mapWithAdjacencyList:
            /*lazy*/ val diGraphMapReversed2:
            Map[Int, Set[Int]] =
              makeTransposeSetsMapFromSetsMap(
                                               sourceMap = diGraphMap,
                                               unReversed =
                                                 unReversedBitSet
                                             )
            lazy val endTime3 = Calendar.getInstance().getTime()
            lazy val endStampString3 =
              timeStampFormat
              .format(endTime3)
            println(s"Done at:" + endStampString3)
            /*lazy*/ val endTimeStamp3: Long = System.currentTimeMillis()
            //println(s"Done at 'endTimeStamp3':" + endTimeStamp3)
            println(
                     s"Time difference is:" +
                       (endTimeStamp3 - startTimeStamp3) + " Millis, or :" +
                       convertLongToTimeString(
                                                timeNumberMillis =
                                                  endTimeStamp3 -
                                                    startTimeStamp3,
                                                colored = false
                                              ) +
                       s" elapsed"
                   )
            println(
                     s"'arcs` are extracted from file" +
                       //s"\n'diGraphMapReversed2.size` is:" +
                       //diGraphMapReversed2.size +
                       s"\n'diGraphMapReversed2` first 3 elements are:\n" +
                       diGraphMapReversed2
                       .view
                       .take(3)
                       .map(
                       { case (k, v) =>
                         k + "" +
                           v.mkString("{", ",", "}")
                       }
                           )
                       .mkString(",")
                   )
            assume(
                    //true == true,
                    diGraphMapReversed2
                    .nonEmpty &&
                      diGraphMapReversed2 == diGraphMapReversed,
                    //.size == expectedSCCsInDiGraph,
                    s"\n'diGraphMapReversed2' must be " +
                      s"same as 'diGraphMapReversed'"
                  )
          }
  /*
  DONE try to write intermediate results on disk, then read them from here
  in particular:
  'diGraphMapReversed' as string lines copy (one line per node with adjusted),
  Max 'adjustedNodes.size' in 'mapWithAdjacencyList' is:'456'
  'graphPostOrder' as stream of Int (one line per value)
  '875714' lines total
   */
  test(
          "82: " +
            //"'writeAdjustedListToTextFile' " +
            "'writeAdjustedListToTextFileByChunk' " +
            "should " +
            "write graph's Adjusted List To Text File"
        ) {
            val startFromNode: Int =
            //8
              7
            val sourceSize: Int = 5105043
            val expectedNodesSize: Int = 875714
            val expectedArcsSize: Int = 5105043
            /*all 'nodes', but only few / some 'arcs'*/
            /*at least as big as `mockUp(14)`*/
            val inputTakeNumber: Int =
              expectedArcsSize
            //500000
            //250000 +
            /*125000 +
            62500 +
            //31250 +
            15625 +
            //7812 +
            //3906 +
            1953 +
            //976 +
            //488 +
            //244 +
            //122
            //61 +
            //30 +
            //15 +
            7*/

            val expectedNodesInSCC: Int = 3
            val expectedSCCsInDiGraph: Int = 5
            val expectedSize: Int = 4
            val outPutFilePath: String =
              "/home/gluk-alex/Documents/"
                  val outPutFileName: String =
                    "graphPostOrder.txt"
                  //"diGraphMapReversed.txt"
                  val inPutFilePath: String =
            //"/home/gluk-alex/Documents/sbt_projects/"
              "/media/gluk-alex/" +
                "GDI/Java/Scala/sbt/projects/" +
                "stanfordAlgorithmsDesignAndAnalysis1/" +
                "src/test/scala/" +
                "testSCC/"
            val inPutFileName: String =
            //"SCC.txt"
            "tinyDG.txt"
            //"tinyDAG.txt"
              //"diGraphWith4SCCs"
            lazy val (actualFileContent, fileContentIter):
            (Iterator[String], Iterator[String]) =
            //Iterator.empty
              readFromFile(
                            fileName = inPutFileName,
                            filePath = inPutFilePath
                          )
              /*reduce / control input size*/
              //.take(inputTakeNumber)
              .duplicate

            /*if (inputTakeNumber >= expectedArcsSize) {
              println(s"fetching all($expectedArcsSize) available arcs")
            }*/

            /*
            Calling 'head' on a `buffered` `iterator`
            will return
            its first `element` but
            will not `advance` the `iterator`.
             */
            /*val bit: BufferedIterator[String] =
              fileContentIter
              .buffered
            def skipEmptyWords(it: BufferedIterator[String]) =
              while (it.head.isEmpty) {it.next()}*/

            val nodesInGraph: Int =
              if (inPutFileName == "SCC.txt") {
                expectedNodesSize
                //875714
              } else {
                actualFileContent.next().toInt
              }
            val edgesInGraph: Int =
              if (inPutFileName == "SCC.txt") {
                expectedArcsSize
                //5105043
              } else {
                actualFileContent.next().toInt
              }
            println(
                     s"\ntotal 'nodesInGraph`:$nodesInGraph" +
                       s"\ntotal 'edgesInGraph`:$edgesInGraph" //+
                     //s"\ntotal 'adjusted` for node '1':" +
                     //List(1,2,5,6,7,3,8,4,47646,47647,13019,47648,47649,
                     // 47650,7700,47651,47652,511596,1,9,10,11,12,13,14).size +
                     //s"\n'firstFiveStrIter`:"+
                     //firstFiveStrIter.mkString(",")
                   )

            /*only '5' max matter*/
            val takeNumber: Int = 5
            //val nodesLimit: Int = Int.MinValue

            lazy val startTime1: java.util.Date =
              Calendar.getInstance().getTime()
            lazy val timeStamp1: Long = System.currentTimeMillis
            /*Date and Time Pattern */
            val timeStampFormat = new SimpleDateFormat("HH:mm:ss.SSS")
            lazy val startStampString1 = timeStampFormat.format(startTime1)
            println(s"'makeSetsMapFromArcs' started at:" +
                      startStampString1)

            lazy val timeStamp3: Long = System.currentTimeMillis
            lazy val diGraphMapReversed:
            Map[Int, Set[Int]] =
              makeSetsMapFromArcs(
                                   fileContentIter =
                                     fileContentIter//,
                                   //.drop(2)
                                   /*for 'diGraphMap'*/
                                   //nonReversedArcs = false
                                 )
            lazy val timeStamp4: Long = System.currentTimeMillis()
            lazy val endTime1: java.util.Date =
              Calendar.getInstance().getTime()
            lazy val endStampString1 = timeStampFormat.format(endTime1)
            println(s"'makeSetsMapFromArcs' Done at:" + endStampString1)
            println(s"time difference is:" +
                      (timeStamp4 - timeStamp3) + " Millis or:" +
                      convertLongToTimeString(
                                               timeNumberMillis =
                                                 timeStamp4 - timeStamp3,
                                               colored = false
                                             )
                   )

            /*println(
                     s"'arcs` are extracted from file" +
                       //s"\n'diGraphMapReversed.size` is:\n" +
                       //diGraphMapReversed.size +
                       s"\n'diGraphMapReversed` first 15 elements are:\n" +
                       diGraphMapReversed
                       .view
                       .take(15)
                       .map(
                       /*(k,v): (Int,NodeMapValFieldsStatic) => */
                       { case (k, v) =>
                         k + "" +
                           v.adjustedNodes.mkString("{", ",", "}")
                       }
                           )
                       .mkString(",") // +
                     /*s"\n'diGraphArray.nodes.head' is:" +
                     diGraphArray.nodes.head +
                     s"\n'diGraphArray.nodes.tail.head' is:" +
                     diGraphArray.nodes.tail.head*/
                   )*/

            /*lazy*/ val startTime2: java.util.Date =
              Calendar.getInstance().getTime()
            /*lazy*/ val startStampString2 =
              timeStampFormat.format(startTime2)
            /*lazy*/ val startTimeStamp5: Long =
              System
              .currentTimeMillis
            println(
                     s"'writeAdjustedListToTextFile' " +
                       s"Start at:" +
                       //startTimeStamp5
                       startStampString2
                   )

            /*
            DONE batch mode appending to existing file will be helpful
             */
            /*side effect*/
            /*writeAdjustedListToTextFile(
                                         outPutFilePath =
                                           "/home/gluk-alex/Documents/",
                                         outPutFileName =
                                           //"diGraphMapReversed.txt",
                                           "diGraphMap.txt",
                                         adjacencyList = diGraphMapReversed
                                       )*/
                  writeAdjustedListToTextFileByChunk(
                                                      filePath =
                                                        outPutFilePath,
                    //"/home/gluk-alex/Documents/",
                  fileName =
                                                        outPutFileName,
                  //"graphPostOrder.txt",
                  //"diGraphMapReversed.txt",
                  adjacencyList = diGraphMapReversed,
                  chunkSize = 5
                  )

            /*lazy*/ val endTimeStamp5: Long = System.currentTimeMillis()
            /*lazy*/ val endTime2: java.util.Date =
              Calendar.getInstance().getTime()
            lazy val endStampString2 =
              timeStampFormat
              .format(endTime2)
            println(s"Done at:" + endStampString2)
            println(
                     s"Time difference is:" +
                       (endTimeStamp5 - startTimeStamp5) + " Millis, or :" +
                       convertLongToTimeString(
                                                timeNumberMillis =
                                                  endTimeStamp5 -
                                                    startTimeStamp5,
                                                colored = false
                                              ) +
                       s" elapsed"
                   )
            lazy val actualFileContentIter:
            Iterator[String] =
            //Iterator.empty
              readFromFile(
                            filePath =
                              outPutFilePath,
                              //"/home/gluk-alex/Documents/",
                            fileName =
                              outPutFileName
                              //"diGraphMapReversed.txt"
                  //"graphPostOrder.txt"
                          )

            assume(
                    actualFileContentIter
                    .nonEmpty &&
                      actualFileContentIter
                      .size == diGraphMapReversed.size,
                    s"\n'actualFileContentIter' must be 'nonEmpty' " +
                      s"& 'source' size equal to 'actualFileContentIter'"
                  )
          }
  ignore(
          "83: 'makeSetsMapFromNodesWithAdjusted' " +
            "should " +
            "make graph from nodes stored in Text File"
        ) {
            val sourceSize: Int = 5105043
            val expectedNodesSize: Int = 875714
            val expectedArcsSize: Int = 5105043
            /*all 'nodes', but only few / some 'arcs'*/
            /*at least as big as `mockUp(14)`*/
            val inputTakeNumber: Int =
              expectedArcsSize
            //500000
            //250000 +
            /*125000 +
            62500 +
            //31250 +
            15625 +
            //7812 +
            //3906 +
            1953 +
            //976 +
            //488 +
            //244 +
            //122
            //61 +
            //30 +
            //15 +
            7*/
            val expectedSize: Int = 4
            val filePath: String =
              "/home/gluk-alex/Documents/"
            //"/home/gluk-alex/Documents/sbt_projects/"
            /*"/media/gluk-alex/" +
              "GDI/Java/Scala/sbt/projects/" +
              "stanfordAlgorithmsDesignAndAnalysis1/" +
              "src/test/scala/" +
              "testSCC/"*/
            val fileName: String =
              "diGraphMapReversed.txt"
            //"SCC.txt"
            //"tinyDG.txt"
            //"tinyDAG.txt"
            //"diGraphWith4SCCs"
            lazy val (actualFileContent, fileContentIter):
            (Iterator[String], Iterator[String]) =
            //Iterator.empty
              readFromFile(
                            fileName = fileName,
                            filePath = filePath
                          )
              /*reduce / control input size*/
              //.take(inputTakeNumber)
              .duplicate
            val nodesInGraph: Int =
              expectedNodesSize
            //875714
            val edgesInGraph: Int =
              expectedArcsSize
            //5105043
            println(
                     s"\ntotal 'nodesInGraph`:$nodesInGraph" +
                       s"\ntotal 'edgesInGraph`:$edgesInGraph"
                   )

            val takeNumber: Int = 5
            lazy val startTime1: java.util.Date =
              Calendar.getInstance().getTime()
            lazy val timeStamp1: Long = System.currentTimeMillis
            /*Date and Time Pattern */
            val timeStampFormat = new SimpleDateFormat("HH:mm:ss.SSS")
            lazy val startStampString1 = timeStampFormat.format(startTime1)
            println(s"'makeSetsMapFromNodesWithAdjusted' started at:" +
                      startStampString1)

            /*lazy*/ val timeStamp3: Long = System.currentTimeMillis
            lazy val diGraphMapReversed:
            Map[Int, Set[Int]] =
              makeSetsMapFromNodesWithAdjusted(
                                                fileContentIter =
                                                  //fileContentIter
                                                  actualFileContent
                                              )
            /*lazy*/ val timeStamp4: Long = System.currentTimeMillis()
            /*lazy*/ val endTime1: java.util.Date =
              Calendar.getInstance().getTime()
            lazy val endStampString1 = timeStampFormat.format(endTime1)
            println(s"'makeSetsMapFromNodesWithAdjusted' Done at:" +
                      endStampString1)
            println(s"time difference is:" +
                      (timeStamp4 - timeStamp3) + " Millis or:" +
                      convertLongToTimeString(
                                               timeNumberMillis =
                                                 timeStamp4 - timeStamp3,
                                               colored = false
                                             )
                   )

            println(
                     s"'nodes` are extracted from file" +
                       //s"\n'diGraphMapReversed.size` is:\n" +
                       //diGraphMapReversed.size +
                       s"\n'diGraphMapReversed` first 15 elements are:\n" +
                       diGraphMapReversed
                       .view
                       .take(15)
                       .map(
                       { case (k, v) =>
                         k + "" +
                           v.mkString("{", ",", "}")
                       }
                           )
                       .mkString(",") // +
                     /*s"\n'diGraphArray.nodes.head' is:" +
                     diGraphArray.nodes.head +
                     s"\n'diGraphArray.nodes.tail.head' is:" +
                     diGraphArray.nodes.tail.head*/
                   )

            assume(
                    diGraphMapReversed
                    .nonEmpty &&
                      fileContentIter
                      .size == diGraphMapReversed.size,
                    s"\n'actualFileContentIter' must be 'nonEmpty' " +
                      s"& 'source' size equal to 'actualFileContentIter'"
                  )
          }
  /*
  TODO replace 'makeSetsMapFromArcs' with 'makeSetsMapFromNodesWithAdjusted'
   */
  ignore(
          "84: 'writeStreamToTextFile' " +
            "should " +
            "write 'postOrder' of graph's nodes to file"
        ) {
            val sourceSize: Int = 5105043
            val expectedNodesSize: Int = 875714
            val expectedArcsSize: Int = 5105043
            val inputTakeNumber: Int =
              expectedArcsSize
            //500000
            //250000 +
            /*125000 +
            62500 +
            //31250 +
            15625 +
            //7812 +
            //3906 +
            1953 +
            //976 +
            //488 +
            //244 +
            //122
            //61 +
            //30 +
            //15 +
            7*/
            val expectedNodesInSCC: Int = 3
            val expectedSCCsInDiGraph: Int = 5
            val expectedSize: Int = 4
            val filePath: String =
            //"/home/gluk-alex/Documents/"
            "/home/gluk-alex/Documents/sbt_projects/"
              /*"/media/gluk-alex/" +
                "GDI/Java/Scala/sbt/projects/" +
                "stanfordAlgorithmsDesignAndAnalysis1/" +
                "src/test/scala/" +
                "testSCC/"*/
            val fileName: String =
            //"graphPostOrder.txt"
            "SCC.txt"
            //"tinyDG.txt"
            //"tinyDAG.txt"
              //"diGraphWith4SCCs"
            lazy val (actualFileContent, fileContentIter):
            (Iterator[String], Iterator[String]) =
            //Iterator.empty
              readFromFile(
                            fileName = fileName,
                            filePath = filePath
                          )
              /*reduce / control input size*/
              //.take(inputTakeNumber)
              .duplicate
            val nodesInGraph: Int =
              if (fileName == "SCC.txt") {
                expectedNodesSize
                //875714
              } else {
                actualFileContent.next().toInt
              }
            val edgesInGraph: Int =
              if (fileName == "SCC.txt") {
                expectedArcsSize
                //5105043
              } else {
                actualFileContent.next().toInt
              }
            println(
                     s"\ntotal 'nodesInGraph`:$nodesInGraph" +
                       s"\ntotal 'edgesInGraph`:$edgesInGraph" //+
                     //s"\ntotal 'adjusted` for node '1':" +
                     //List(1,2,5,6,7,3,8,4,47646,47647,13019,47648,47649,
                     // 47650,7700,47651,47652,511596,1,9,10,11,12,13,14).size +
                     //s"\n'firstFiveStrIter`:"+
                     //firstFiveStrIter.mkString(",")
                   )

            /*only '5' max matter*/
            val takeNumber: Int = 5
            //val nodesLimit: Int = Int.MinValue
            lazy val startTime: java.util.Date =
              Calendar.getInstance().getTime()
            lazy val timeStamp1: Long = System.currentTimeMillis
            /*Date and Time Pattern */
            val timeStampFormat = new SimpleDateFormat("HH:mm:ss.SSS")
            lazy val startStampString = timeStampFormat.format(startTime)
            println(s"'makeAdjacencyListMapFromArcs' started at:" +
                      startStampString)

            //val mapWithAdjacencyList:
            /*lazy*/ val diGraphMap:
            Map[Int, Set[Int]] =
              makeSetsMapFromArcs(
                                   fileContentIter =
                                     actualFileContent
                                 )
            lazy val endTime1 = Calendar.getInstance().getTime()
            lazy val timeStamp2: Long = System.currentTimeMillis()
            lazy val endStampString1 = timeStampFormat.format(endTime1)
            println(s"Done at:" + endStampString1)
            println(s"time difference is:" +
                      (timeStamp2 - timeStamp1) + " Millis or:" +
                      convertLongToTimeString(
                                               timeNumberMillis =
                                                 timeStamp2 - timeStamp1,
                                               colored = false
                                             )
                   )
            /*println(
                     s"'arcs` are extracted from file" +
                       //s"\ninitial 'diGraphMap.size` is:\n" +
                       //diGraphMap.size +
                       s"\n'diGraphMap` first '15' elements are:\n" +
                       diGraphMap
                       //.values
                       .view
                       .take(15)
                       .map(
                       /*(k,v): (Int,NodeMapValFieldsStatic) =>*/
                        { case (k, v) =>
                         k + "" +
                           v.adjustedNodes.mkString("{", ",", "}")
                       }
                           )
                       .mkString(",") //+
                     /*s"\n'diGraphArray.nodes.head' is:" +
                     diGraphArray.nodes.head +
                     s"\n'diGraphArray.nodes.tail.head' is:" +
                     diGraphArray.nodes.tail.head*/
                   )*/
            /*lazy*/ val startTime2: java.util.Date =
              Calendar.getInstance().getTime()
                  /*lazy*/ val startStampString2 =
                    timeStampFormat.format(startTime2)
            /*lazy*/ val startTimeStamp2: Long = System.currentTimeMillis
            println(s"'postOrderOnMap' Start at:" +
                      startStampString2)
            lazy val graphPostOrder:
            //Iterable[List[Int]] =
            //List[Int] =
            Stream[Int] =
              postOrderOnMap(
                              //diGraphMapReversed
                              diGraphMap /*,
                            Some(0)*/)
              .view
              //.sorted(Ordering[Int].reverse)
              //.take(takeNumber)
              //.toList
              //.reverse
              .toStream

            /*lazy*/ val endTimeStamp2: Long = System.currentTimeMillis()
                  /*lazy*/ val endTime2: java.util.Date =
                    Calendar.getInstance().getTime()
                  lazy val endStampString2 =
                    timeStampFormat
                    .format(endTime2)
            println(s"Done at:" + endStampString2)
            println(
                     s"Time difference is:" +
                       (endTimeStamp2 - startTimeStamp2) + " Millis, or :" +
                       convertLongToTimeString(
                                                timeNumberMillis =
                                                  endTimeStamp2 -
                                                    startTimeStamp2,
                                                colored = false
                                              ) +
                       s" elapsed"
                   )
            println(
                     //s"\n'graphPostOrder.size' is:" +
                     //graphPostOrder.size +
                     s"\n'graphPostOrder' is:\n${
                       graphPostOrder
                       .take(15)
                       .mkString(",")
                     }"
                   )

            /*lazy*/ val startTime3: java.util.Date =
              Calendar.getInstance().getTime()
            /*lazy*/ val startStampString3 =
              timeStampFormat.format(startTime3)
            /*lazy*/ val startTimeStamp3: Long =
              System
              .currentTimeMillis
            println(
                     s"'writeStreamToTextFile' " +
                       s"Start at:" +
                       startStampString3
                   )

            /*side effect*/
            writeStreamToTextFile(
                                   filePath =
                                     "/home/gluk-alex/Documents/",
                                   fileName =
                                     "graphPostOrder.txt",
                                   sourceStream = graphPostOrder
                                 )

            /*lazy*/ val endTimeStamp3: Long =
              System.currentTimeMillis()
            /*lazy*/ val endTime3: java.util.Date =
              Calendar.getInstance().getTime()
            lazy val endStampString3 =
              timeStampFormat
              .format(endTime3)
            println(s"Done at:" + endStampString3)
            println(
                     s"Time difference is:" +
                       (endTimeStamp3 - startTimeStamp3) + " Millis, or :" +
                       convertLongToTimeString(
                                                timeNumberMillis =
                                                  endTimeStamp3 -
                                                    startTimeStamp3,
                                                colored = false
                                              ) +
                       s" elapsed"
                   )
            lazy val actualFileContentIter:
            Iterator[String] =
            //Iterator.empty
              readFromFile(
                            filePath = "/home/gluk-alex/Documents/",
                            fileName = "diGraphMapReversed.txt"
                          )
            assume(
                    actualFileContentIter
                    .nonEmpty &&
                      actualFileContentIter
                      .size == graphPostOrder.size,
                    s"\n'actualFileContentIter' must be 'nonEmpty' " +
                      s"& actualFileContentIter.size equal to " +
                      s"'graphPostOrder.size'"
                  )
          }
  /*assume("diGraphMapReversed.txt" exist with right content)*/
  /*assume("graphPostOrder.txt" exist with right content)*/
  ignore(
        "85: 'makeSetsMapFromNodesWithAdjusted' " +
          "then 'iterativeDFS_OnMap'" +
          "should return " +
          "all SCCs in graph"
      ) {
          val sourceSize: Int = 5105043
          val expectedNodesSize: Int = 875714
          val expectedArcsSize: Int = 5105043
          /*all 'nodes', but only few / some 'arcs'*/
          /*at least as big as `mockUp(14)`*/
          val inputTakeNumber: Int =
            expectedArcsSize
          //500000
          //250000 +
          /*125000 +
          62500 +
          //31250 +
          15625 +
          //7812 +
          //3906 +
          1953 +
          //976 +
          //488 +
          //244 +
          //122
          //61 +
          //30 +
          //15 +
          7*/
          val expectedSize: Int = 4
          val filePath: String =
            "/home/gluk-alex/Documents/"
          //"/home/gluk-alex/Documents/sbt_projects/"
          /*"/media/gluk-alex/" +
            "GDI/Java/Scala/sbt/projects/" +
            "stanfordAlgorithmsDesignAndAnalysis1/" +
            "src/test/scala/" +
            "testSCC/"*/
          val fileName: String =
            "diGraphMapReversed.txt"
          //"SCC.txt"
          //"tinyDG.txt"
          //"tinyDAG.txt"
          //"diGraphWith4SCCs"
          lazy val (actualFileContent, fileContentIter):
          (Iterator[String], Iterator[String]) =
          //Iterator.empty
            readFromFile(
                          fileName = fileName,
                          filePath = filePath
                        )
            /*reduce / control input size*/
            //.take(inputTakeNumber)
            .duplicate
          val nodesInGraph: Int =
            expectedNodesSize
          //875714
          val edgesInGraph: Int =
            expectedArcsSize
          //5105043
          println(
                   s"\ntotal 'nodesInGraph`:$nodesInGraph" +
                     s"\ntotal 'edgesInGraph`:$edgesInGraph"
                 )

          val takeNumber: Int = 5
          /*lazy*/ val startTime1: java.util.Date =
            Calendar.getInstance().getTime()
          /*lazy*/ val startTimeStamp1: Long =
            System.currentTimeMillis
          /*Date and Time Pattern */
          val timeStampFormat = new SimpleDateFormat("HH:mm:ss.SSS")
          lazy val startStampString1 =
            timeStampFormat
            .format(startTime1)
          println(s"'makeSetsMapFromNodesWithAdjusted' started at:" +
                    startStampString1)
          lazy val diGraphMapReversed:
          Map[Int, Set[Int]] =
            makeSetsMapFromNodesWithAdjusted(
                                              fileContentIter =
                                                //fileContentIter
                                                actualFileContent
                                            )
          /*lazy*/ val endTimeStamp1: Long =
            System.currentTimeMillis()
          /*lazy*/ val endTime1: java.util.Date =
            Calendar.getInstance().getTime()
          lazy val endStampString1 = timeStampFormat.format(endTime1)
          println(s"'makeSetsMapFromNodesWithAdjusted' Done at:" +
                    endStampString1)
          println(s"time difference is:" +
                    (endTimeStamp1 - startTimeStamp1) + " Millis or:" +
                    convertLongToTimeString(
                                             timeNumberMillis =
                                               endTimeStamp1 -
                                                 startTimeStamp1,
                                             colored = false
                                           )
                 )

          println(
                   s"'nodes` are extracted from file" +
                     //s"\n'diGraphMapReversed.size` is:\n" +
                     //diGraphMapReversed.size +
                     s"\n'diGraphMapReversed` first 15 elements are:\n" +
                     diGraphMapReversed
                     .view
                     .take(15)
                     .map(
                     { case (k, v) =>
                       k + "" +
                         v.mkString("{", ",", "}")
                     }
                         )
                     .mkString(",") // +
                   /*s"\n'diGraphArray.nodes.head' is:" +
                   diGraphArray.nodes.head +
                   s"\n'diGraphArray.nodes.tail.head' is:" +
                   diGraphArray.nodes.tail.head*/
                 )

          /*lazy*/ val startTime2: java.util.Date =
            Calendar.getInstance().getTime()
          /*lazy*/ val startTimeStamp2: Long =
            System.currentTimeMillis
          lazy val startStampString2 =
            timeStampFormat
            .format(startTime2)
          println(s"read 'graphPostOrder' from file started at:" +
                    startStampString2)
          val graphPostOrder: Stream[Int] =
            readFromFile(
                          filePath = "/home/gluk-alex/Documents/",
                          fileName = "graphPostOrder.txt"
                        )
            .toStream
            .view
            .map(_.toInt)
            .toStream
          /*lazy*/ val endTimeStamp2: Long =
            System.currentTimeMillis()
          /*lazy*/ val endTime2: java.util.Date =
            Calendar.getInstance().getTime()
          lazy val endStampString2 =
            timeStampFormat.format(endTime2)
          println(s"Done at:" +
                    endStampString2)
          println(s"time difference is:" +
                    (endTimeStamp2 - startTimeStamp2) + " Millis or:" +
                    convertLongToTimeString(
                                             timeNumberMillis =
                                               endTimeStamp2 -
                                                 startTimeStamp2,
                                             colored = false
                                           )
                 )

          val startTime3: java.util.Date =
            Calendar.getInstance().getTime()
          /*lazy*/ val startTimeStamp3: Long =
            System.currentTimeMillis
          lazy val startStampString3 =
            timeStampFormat
            .format(startTime2)
          println(
                   s"Starting collecting 'allSCCs' ... at:" +
                     startStampString3
                 )
          /*
          DONE replace 'Iterable[List[Int]]' with 'Iterable[Int]'
          because interested only in SCCs size
           */
          //{8},{7,6},{4,5},{3,1,2}
          val allSCCs:
          //Iterable[List[Int]] =
          Iterable[Int] =
          //Stream[Stream[Int]] =
            iterativeDFS_OnMap(
                                adjacencyList =
                                  diGraphMapReversed,
                                //diGraphMap,
                                postOrderNodesStream =
                                  graphPostOrder
                                //reversedGraphPostOrder
                                //.reverse
                              )
            .view
            .sorted(Ordering[Int].reverse)
            //.sortBy(_.length)
            .take(takeNumber)
            .toList
          //.toStream

          /*lazy*/ val endTimeStamp3: Long =
            System.currentTimeMillis()
          /*lazy*/ val endTime3: java.util.Date =
            Calendar.getInstance().getTime()
          lazy val endStampString3 =
            timeStampFormat.format(endTime3)
          println(s"Done at:" +
                    endStampString3)
          println(s"time difference is:" +
                    (endTimeStamp3 - startTimeStamp3) + " Millis or:" +
                    convertLongToTimeString(
                                             timeNumberMillis =
                                               endTimeStamp3 -
                                                 startTimeStamp3,
                                             colored = false
                                           ) +
                    s" elapsed"
                 )
          println(
                   //s"'allSCCs.size' is:" +
                   //allSCCs.size +
                   /*s"\n'mapWithAdjacencyList.head' is:" +
                     directedGraphDynamic
                       .nodesWithAdjusted
                     .head +
                     s"\n'mapWithAdjacencyList.tail.head' is ${
                       directedGraphDynamic
                       .nodesWithAdjusted
                       .tail.head
                     }" +*/
                   //s"\n'nodesWithAdjusted` is:" +
                   s"\n`inputTakeNumber` is $inputTakeNumber" +
                     s"\nfirst $takeNumber elements in " +
                     s"'allSCCs`" +
                     s" are:" +
                     s"\n${
                       allSCCs
                       .take(takeNumber)
                       //.map(n => n.mkString("{", ",", "}"))
                       .mkString(",")
                     }"
                 )

          assume(
                  allSCCs
                  .nonEmpty &&
                    allSCCs
                    //.flatMap(_.map(_ => 1))
                    .sum ==
                      //nodesInGraph,
                      //graphPostOrder.size ||
                    diGraphMapReversed.size,
                  s"\n'allSCCs' must be 'nonEmpty' " +
                    s"& total SCCs size equal to 'nodesInGraph'"
                )
        }

  ignore(
          "86: 'postOrderOnMap' " +
            "then 'iterativeDFS_OnMap'" +
            "should return " +
            "all SCCs in graph"
        ) {
            val startFromNode: Int =
            //8
              7
            val sourceSize: Int = 5105043
            val expectedNodesSize: Int = 875714
            val expectedArcsSize: Int = 5105043
            /*all 'nodes', but only few / some 'arcs'*/
            /*at least as big as `mockUp(14)`*/
            val inputTakeNumber: Int =
              expectedArcsSize
            //500000
            //250000 +
            /*125000 +
            62500 +
            //31250 +
            15625 +
            //7812 +
            //3906 +
            1953 +
            //976 +
            //488 +
            //244 +
            //122
            //61 +
            //30 +
            //15 +
            7*/

            val expectedNodesInSCC: Int = 3
            val expectedSCCsInDiGraph: Int = 5
            val expectedSize: Int = 4
            val filePath: String =
            //"/home/gluk-alex/Documents/sbt_projects/"
              "/media/gluk-alex/" +
                "GDI/Java/Scala/sbt/projects/" +
                "stanfordAlgorithmsDesignAndAnalysis1/" +
                "src/test/scala/" +
                "testSCC/"
            val fileName: String =
            //"SCC.txt"
            //"tinyDG.txt"
            //"tinyDAG.txt"
              "diGraphWith4SCCs"
            /*There is
            only one standard operation
            which allows to
            re-use the same `iterator`:
            The call
             */
            //val (it1, it2) = it.duplicate
            /*gives you
            two `iterators`
            which each return
            exactly the same `elements` as
            the iterator 'it'.
            The two `iterators` work independently;
            advancing one does not affect the other.
            By contrast
            the original iterator 'it' is
            advanced to its `end`
            by `duplicate` and
            is thus rendered `unusable`.
             */
            lazy val (actualFileContent, fileContentIter):
            (Iterator[String], Iterator[String]) =
            //val actualFileContent: Iterator[String] =
            //Iterator.empty
              readFromFile(
                            fileName = fileName,
                            filePath = filePath
                          )
              /*reduce / control input size*/
              //.take(inputTakeNumber)
              .duplicate
            /*val firstFiveStrIter: Iterator[String] =
              readFromFile(
                            fileName = fileName,
                            filePath = filePath
                          )
              .take(25)*/
            /*if (inputTakeNumber >= expectedArcsSize) {
              println(s"fetching all($expectedArcsSize) available arcs")
            }*/

            /*
            Calling 'head' on a `buffered` `iterator`
            will return
            its first `element` but
            will not `advance` the `iterator`.
             */
            /*val bit: BufferedIterator[String] =
              fileContentIter
              .buffered
            def skipEmptyWords(it: BufferedIterator[String]) =
              while (it.head.isEmpty) {it.next()}*/

            val nodesInGraph: Int =
              if (fileName == "SCC.txt") {
                expectedNodesSize
                //875714
              } else {
                actualFileContent.next().toInt
              }
            val edgesInGraph: Int =
              if (fileName == "SCC.txt") {
                expectedArcsSize
                //5105043
              } else {
                actualFileContent.next().toInt
              }
            println(
                     s"\ntotal 'nodesInGraph`:$nodesInGraph" +
                       s"\ntotal 'edgesInGraph`:$edgesInGraph" //+
                     //s"\ntotal 'adjusted` for node '1':" +
                     //List(1,2,5,6,7,3,8,4,47646,47647,13019,47648,47649,
                     // 47650,7700,47651,47652,511596,1,9,10,11,12,13,14).size +
                     //s"\n'firstFiveStrIter`:"+
                     //firstFiveStrIter.mkString(",")
                   )

            /*only '5' max matter*/
            val takeNumber: Int = 5
            //val nodesLimit: Int = Int.MinValue

            lazy val startTime: java.util.Date =
              Calendar.getInstance().getTime()
            lazy val timeStamp1: Long = System.currentTimeMillis
            /*Date and Time Pattern */
            val timeStampFormat = new SimpleDateFormat("HH:mm:ss.SSS")
            lazy val startStampString = timeStampFormat.format(startTime)
            println(s"'makeAdjacencyListMapFromArcs' started at:" +
                      startStampString)
            println(s"timeStamp1:" + timeStamp1)
            //println

            //val mapWithAdjacencyList:
            /*lazy*/ val diGraphMap:
            Map[Int, Set[Int]] =
              makeSetsMapFromArcs(
                                   fileContentIter =
                                     actualFileContent
                                 )
            /*fails in IDE REPL, same as 'startTime'*/
            lazy val endTime = Calendar.getInstance().getTime()
            lazy val timeStamp2: Long = System.currentTimeMillis()
            lazy val endStampString = timeStampFormat.format(startTime)
            val timeDifference: Long =
              timeStamp2 - timeStamp1
            println(s"Done at:" + endStampString)
            println(s"timeStamp2:" + timeStamp2)
            println(s"time difference is:" +
                      (timeStamp2 - timeStamp1) + " Millis")
            println(s"time difference is:" +
                      convertLongToTimeString(
                                               timeNumberMillis =
                                                 timeDifference,
                                               colored = false
                                             )
                   )
            /*println(
                     s"'arcs` are extracted from file" +
                       //s"\ninitial 'diGraphMap.size` is:\n" +
                       //diGraphMap.size +
                       s"\n'diGraphMap` first '15' elements are:\n" +
                       diGraphMap
                       //.values
                       .view
                       .take(15)
                       .map(
                       /*(k,v): (Int,NodeMapValFieldsStatic) =>*/
                        { case (k, v) =>
                         k + "" +
                           v.adjustedNodes.mkString("{", ",", "}")
                       }
                           )
                       .mkString(",") //+
                     /*s"\n'diGraphArray.nodes.head' is:" +
                     diGraphArray.nodes.head +
                     s"\n'diGraphArray.nodes.tail.head' is:" +
                     diGraphArray.nodes.tail.head*/
                   )*/

            lazy val timeStamp3: Long = System.currentTimeMillis
            println(s"Start at 'timeStamp3':" + timeStamp3)
            lazy val diGraphMapReversed:
            Map[Int, Set[Int]] =
              makeSetsMapFromArcs(
                                   fileContentIter =
                                     fileContentIter,
                                   //.drop(2)
                                   nonReversedArcs = false
                                 )
            lazy val timeStamp4: Long = System.currentTimeMillis()
            println(s"Done at 'timeStamp4':" + timeStamp2)
            println(s"time difference is:" +
                      (timeStamp4 - timeStamp3) + " Millis")
            println(s"time difference is:" +
                      convertLongToTimeString(
                                               timeNumberMillis =
                                                 timeStamp4 - timeStamp3,
                                               colored = false
                                             )
                   )
            /*println(
                     s"'arcs` are extracted from file" +
                       //s"\n'diGraphMapReversed.size` is:\n" +
                       //diGraphMapReversed.size +
                       s"\n'diGraphMapReversed` first 15 elements are:\n" +
                       diGraphMapReversed
                       .view
                       .take(15)
                       .map(
                       /*(k,v): (Int,NodeMapValFieldsStatic) => */
                       { case (k, v) =>
                         k + "" +
                           v.adjustedNodes.mkString("{", ",", "}")
                       }
                           )
                       .mkString(",") // +
                     /*s"\n'diGraphArray.nodes.head' is:" +
                     diGraphArray.nodes.head +
                     s"\n'diGraphArray.nodes.tail.head' is:" +
                     diGraphArray.nodes.tail.head*/
                   )*/

            lazy val startTimeStamp5: Long = System.currentTimeMillis
            println(s"Start at 'startTimeStamp5':" + startTimeStamp5)

            /*8,5,6,7,4,2,3,1*/
            //1,3,2,4,5,0,11,9,12,10,8,6,7
            //7,6,8,10,12,9,11,0,5,4,2,3,1
            /*for DAG where each node is SCC*/
            /*8,7,2,3,0,5,1,6,9,11,10,12,4*/
            /*4,12,10,11,9,6,1,5,0,3,2,7,8*/
            lazy val graphPostOrder:
            //Iterable[List[Int]] =
            //List[Int] =
            Stream[Int] =
              postOrderOnMap(
                              //diGraphMapReversed
                              diGraphMap /*,
                            Some(0)*/)
              .view
              //.sorted(Ordering[Int].reverse)
              //.take(takeNumber)
              //.toList
              //.reverse
              .toStream

            lazy val endTimeStamp5: Long = System.currentTimeMillis()
            println(s"Done at 'endTimeStamp5':" + endTimeStamp5)
            println(
                     s"Time difference is:" +
                       (endTimeStamp5 - startTimeStamp5) + " Millis, or :" +
                       convertLongToTimeString(
                                                timeNumberMillis =
                                                  endTimeStamp5 -
                                                    startTimeStamp5,
                                                colored = false
                                              ) +
                       s" elapsed"
                   )
            println(
                     //s"\n'graphPostOrder.size' is:" +
                     //graphPostOrder.size +
                     s"\n'graphPostOrder' is:\n${
                       graphPostOrder
                       .take(15)
                       .mkString(",")
                     }"
                   )

            lazy val startTimeStamp6: Long = System.currentTimeMillis
            println(s"Start at 'startTimeStamp6':" + startTimeStamp6)
            //8,7,6,10,12,9,11,3,5,4,2,0,1
            lazy val reversedGraphPostOrder:
            //Iterable[List[Int]] =
            //List[Int] =
            Stream[Int] =
              postOrderOnMap(diGraphMapReversed /*,Some(0)*/)
            /*.view
            .sorted(Ordering[Int].reverse)
            .take(takeNumber)
            //.toList
            .toStream*/

            lazy val endTimeStamp6: Long = System.currentTimeMillis()
            println(s"Done at 'endTimeStamp6':" + endTimeStamp6)
            println(
                     s"Time difference is:" +
                       (endTimeStamp6 - startTimeStamp6) + " Millis, or :" +
                       convertLongToTimeString(
                                                timeNumberMillis =
                                                  endTimeStamp6 -
                                                    startTimeStamp6,
                                                colored = false
                                              ) +
                       s" elapsed"
                   )
            /*println(
                     //s"\n'graphPostOrder.size' is:" +
                     //graphPostOrder.size +
                     s"\n'reversedGraphPostOrder' is:\n${
                       reversedGraphPostOrder
                       .take(15)
                       .mkString(",")
                     }"
                   )*/

            lazy val startTimeStamp7: Long = System.currentTimeMillis
            println(s"Start at 'startTimeStamp7':" + startTimeStamp7)
            println(
                     s"starting collecting 'allSCCs' ...")
            /*
            DONE replace 'Iterable[List[Int]]' with 'Iterable[Int]'
            because interested only in SCCs size
             */
            val allSCCs:
            //Iterable[List[Int]] =
            Iterable[Int] =
            //Stream[Stream[Int]] =
              iterativeDFS_OnMap(
                                  adjacencyList =
                                    diGraphMapReversed,
                                  //diGraphMap,
                                  postOrderNodesStream =
                                    graphPostOrder
                                  //reversedGraphPostOrder
                                  //.reverse
                                )
              .view
              .sorted(Ordering[Int].reverse)
              //.sortBy(_.length)
              .take(takeNumber)
              .toList
            //.toStream

            lazy val endTimeStamp7: Long = System.currentTimeMillis()
            println(s"Done at 'endTimeStamp7':" + endTimeStamp7)
            println(
                     s"Time difference is:" +
                       (endTimeStamp7 - startTimeStamp7) + " Millis, or :" +
                       convertLongToTimeString(
                                                timeNumberMillis =
                                                  endTimeStamp7 -
                                                    startTimeStamp7,
                                                colored = false
                                              ) +
                       s" elapsed"
                   )
            println(
                     //s"'allSCCs.size' is:" +
                     //allSCCs.size +
                     /*s"\n'mapWithAdjacencyList.head' is:" +
                       directedGraphDynamic
                         .nodesWithAdjusted
                       .head +
                       s"\n'mapWithAdjacencyList.tail.head' is ${
                         directedGraphDynamic
                         .nodesWithAdjusted
                         .tail.head
                       }" +*/
                     //s"\n'nodesWithAdjusted` is:" +
                     s"\n`inputTakeNumber` is $inputTakeNumber" +
                       s"\nfirst $takeNumber elements in " +
                       s"'allSCCs`" +
                       s" are:" +
                       s"\n${
                         allSCCs
                         .take(takeNumber)
                         //.map(n => n.mkString("{", ",", "}"))
                         .mkString(",")
                       }"
                   )

            assume(
                    //true == true,
                    allSCCs
                    .nonEmpty &&
                      allSCCs
                      //.flatMap(_.map(_ => 1))
                      .sum == nodesInGraph,
                    //.size == expectedSCCsInDiGraph,
                    s"\n'allSCCs' must be 'nonEmpty' " +
                      s"& total SCCs size equal to 'nodesInGraph'"
                  )
          }

  ignore(
          "100: 'convert decimal to binary' " +
            "should return " +
            "right representation"
        ) {
            case class ConvertResults(
                                       binaryRepresentation: String,
                                       decimalDecomposition: List[Int],
                                       remainders: List[Int]
                                       )

            val decimalNumber: Int = 2014
            val expectedResult: String =
            //Int.toString(decimalNumber,2)
            //*Integer.toString(decimalNumber,2)
              decimalNumber.toBinaryString
            //BigDecimal(decimalNumber).byteValue().toString

            /*
            pc.aggregate(Set[Int]())(_ += process(_), _ ++ _)
            Aggregates the results of
            applying an operator to subsequent elements.
             */
            /*`positional` notation*/
            /*?must run at least once for non empty input
            ?so, post condition as `until` ?
            first iteration is special because
            number value checked unchanged, not halved
            * */
            @scala.annotation.tailrec
            def convertDecToBin(
                                 decNum: Int,
                                 /*halfs w/o remainders & initial value*/
                                 resultsSeqInt:
                                 List[Int] =
                                 //List[String] =
                                 List.empty,
                                 remainders:
                                 List[Int] =
                                 //List[String] =
                                 List.empty,
                                 resultsStr:
                                 String = "",
                                 conditionUntil: Boolean = false
                                 ): ConvertResults = {
              //): (String, List[Int]) = {
              //): String = {
              if (
              /*decNum == 0 ||
              decNum == 1*/
                conditionUntil
              ) {
                /*val rightmostPosition: String =
                  resultsSeqInt.last match {
                    case odd if odd % 2 != 0 =>"1"
                    case even                => "0"}*/
                /*return value*/
                //(resultsStr, resultsSeqInt)
                ConvertResults(
                                //resultsStr + rightmostPosition,
                                resultsStr,
                                resultsSeqInt,
                                remainders)
                //.reduce()
                /*.fold("")(_ + _ match {
                  case odd if _ % 2 != 0 => "1"
                  case even => "0"
                })*/
                /*.map(/*_ match*/ {
                       case odd if odd % 2 != 0 => "1"
                       case even => "0"
                     })*/
                //.mkString
              } else {
                val half: Int =
                  if (resultsSeqInt.isEmpty) {
                    /*initial value*/
                    decNum
                  } else {
                    decNum / 2
                  }
                val remainder: Int =
                  half % 2
                //decNum % 2
                //val elemToAdd: String =
                val (strUpdated, intSeqUpdated): (String, List[Int]) =
                  half match {
                    case odd if odd % 2 != 0 =>
                      ("1" + resultsStr,
                        odd +: resultsSeqInt)
                    case even                =>
                      ("0" + resultsStr,
                        even +: resultsSeqInt)
                  }
                val checkedCondition: Boolean =
                /*decNum == 0 ||
                  decNum == 1*/
                  half == 1
                /*recursion*/
                convertDecToBin(
                                 half,
                                 resultsStr = strUpdated,
                                 resultsSeqInt = intSeqUpdated,
                                 remainders = remainder +: remainders,
                                 conditionUntil = checkedCondition
                               )
              }
            }

            //val binaryString: String =
            //val (binaryString, intSource): (String, List[Int]) =
            val ConvertResults(
            binaryString,
            intSource,
            remainders
                              ): ConvertResults =
            //): (String, List[Int]) =
              convertDecToBin(decimalNumber)
            val halvesSum: Int = intSource.init.sum
            val remaindersSum: Int = remainders.sum
            val remaindersExpected: List[Int] =
              intSource
              .init
              .map(_ % 2)
            val totalSum: Int = halvesSum + remaindersSum

            println(
                     s"\n`$decimalNumber` decompose to halves is:\n" +
                       intSource +
                       s"\n`remainders` are:\n" +
                       remainders +
                       s"\n`remaindersExpected` are:\n" +
                       remaindersExpected +
                       s"\n'intSource.sum' is:" +
                       halvesSum +
                       s"\n'remainders.sum' is:" +
                       remaindersSum +
                       s"\n'totalSum' is:" +
                       totalSum +
                       s"\n$decimalNumber 'convertDecToBin' is:" +
                       binaryString +
                       s"\n'convertDecToBin.length' is:" +
                       binaryString.length +
                       s"\n'expectedResult.length' is:" +
                       expectedResult.length
                   )
            assume(
                    binaryString == expectedResult,
                    s"\n'allSCCs' must be 'nonEmpty' " +
                      s"& equal to 'expectedResult'"
                  )

          }
  ignore(
          "101: 'progress print' " +
            "should return " +
            "one changing string printed in Console"
        ) {
            println(
                     s"\n`progress` must be shown below:"
                   )
            //val screen = new ConsoleWriter
            val placeHolderStr: String = "Start[" + " " * 10 + "]"
            /*System.out
            .*/ print(placeHolderStr)
            //System.out.flush()
            Thread.sleep(50L)
            /*!!! Warn !!! 'scalatest' ignored console colors
            when runs in IDE
            IDE affected side effects like terminal output
            use SBT instead
            * */
            (1 to 10)
            .foreach(i => {
              print("")
              Thread.sleep(150L)
              print(
                     s"Start[${Console.RESET}" +
                       s"${Console.YELLOW_B}${Console.RED}>" * i +
                       /*s">" * i +
                       s" " * (10 - i) + s"\r"*/
                       s"${Console.RESET} " * (10 - i) +
                       s"${Console.RESET}]\r"
                   )
              Thread.sleep(150L)
            })
            //implicit val ec = scala.concurrent.ExecutionContext.global
            val progressCollected: IndexedSeq[Int] =
              for (i <- 1 to 10) yield {
                print("")
                //wait(50L)
                lazy val timeConsumingComputation =
                /*scala.concurrent
                .Future*/ {
                  Thread.sleep(250L)
                  print(s"%" * i + s" " * (10 - i) + s"\r")
                }
                /*scala.concurrent.
                Await
                .result(
                    timeConsumingComputation,
                    scala.concurrent.duration.Duration(500, "millis"))*/
                /*return*/
                i
              }
            println
            assume(
                    progressCollected.nonEmpty,
                    s"\n'allSCCs' must be 'nonEmpty' " +
                      s"& equal to 'expectedResult'"
                  )

          }
  ignore(
          "102: 'extractDigitsFromString' " +
            "should return " +
            "list of digits or empty if fails"
        ) {
            val nullChar = "\u0000"
            val backspaceChar = "\u0008"
            /*\t*/
            val charTabulationChar = "\u0009"
            val lineTabulationChar = "\u000B"
            /*? \n ?*/
            val lineFeedChar = "\u000A"
            /*? \r ?*/
            val carriageReturnChar = "\u000D"
            val spaceChar = "\u0020"

            val inputSample: String =
            //" 9 10"
              """13
                |22
                | 4  2
                | 2  3
                | 3  2
                | 6  0
                | 0  1
                | 2  0
                |11 12
                |12  9
                | 9 10""".stripMargin('|') +
                "\n\t" +
                "\n\r" +
                "\n "
            val expectedSize: Int =
              2 +
                9 * 2
            val expectedElement: Int = 10
            val extractResults:
            //Buffer[Int] =
            List[Int] =
            //inputSample
            //.map()
              extractDigitsFromString(inputSample)
              .toList
            println(s"\n" + extractResults.mkString("{", ",", "}"))
            println(s"total:" + extractResults.length + s" elements")

            assume(
                    extractResults.nonEmpty &&
                      extractResults.length == expectedSize &&
                      extractResults.last == expectedElement,
                    s"\n'extractDigitsFromString' must be 'nonEmpty' " +
                      s"& have size equal to 'expectedResult'"
                  )

          }

}
