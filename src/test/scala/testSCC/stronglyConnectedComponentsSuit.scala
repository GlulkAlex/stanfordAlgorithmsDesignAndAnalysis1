package testSCC

import filesIO.FilesIO._

import scala.collection.BitSet
import scala.util.matching.Regex

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

            /*!!!Warn: 'arcs' must be sorted by 'arcTail'!!!*/
            val mapWithAdjacencyList: Map[Int, NodeMapValFieldsStatic] =
              makeAdjacencyListMapFromArcs(
                                            fileContentIter = actualFileContent,
                                            pattern =
                                              """\d+""".r
                                          )
            println(
                     s"\n'mapWithAdjacencyList.head' is:" +
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
                       }"
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
  test(
        "68: 'traverse' " +
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

}
