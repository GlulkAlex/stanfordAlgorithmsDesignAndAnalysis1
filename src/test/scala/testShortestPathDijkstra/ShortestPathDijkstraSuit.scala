package testShortestPathDijkstra

import org.scalatest.FunSuite

import java.text.SimpleDateFormat
import java.util.Calendar

import filesIO.FilesIO._
import shortestPathByDijkstra.ShortestPathDijkstra._

import scala.collection.mutable.Map

/**
 * Created by gluk-alex on 8/29/15.
 */
class ShortestPathDijkstraSuit
  extends FunSuite {
  ignore(
          "1: 'makeWeightedEdgesAndSetsMapsFromNodesWithAdjusted'" +
            "should " +
            "extract weighted edges and " +
            "adjusted nodes list"
        ) {
            val destinationsFromFirstOne:
            List[Int] =
              List(7, 37, 59, 82, 99, 115, 133, 165, 188, 197)
            val answerSample: String =
              "1000,1000,1000,1000,1000,2000,1000,1000,1000,1000"

            val inputTakeNumber: Int =
              3
            val inPutFilePath: String =
            //"/home/gluk-alex/Documents/sbt_projects/"
              "/media/gluk-alex/" +
                "GDI/Java/Scala/sbt/projects/" +
                "stanfordAlgorithmsDesignAndAnalysis1/" +
                "src/test/scala/" +
                //"testSCC/"
                //"testMedianMaintenance/"
                //"testTwoSumAlgorithm/"
                "testShortestPathDijkstra/"
            val inPutFileName: String =
              "dijkstraData.txt"
            lazy val (fileContentIter, fileContentIterCopy):
            (Iterator[String], Iterator[String]) =
            //Iterator.empty
              readFromFile(
                            fileName = inPutFileName,
                            filePath = inPutFilePath
                          )
              /*reduce / control input size*/
              .take(inputTakeNumber)
              .duplicate
            val expectedInputSize: Int =
              3
            val (setsMap,
            edgesMap):
            (collection.mutable.Map[Int, Set[Int]],
              collection.mutable.Map[Edge, Int]) =
              makeWeightedEdgesAndSetsMapsFromNodesWithAdjusted(
                                                                 fileContentIter =
                                                                   fileContentIter
                                                               )
            println(
                     "first 10 'setsMap's elements are:\n" +
                       setsMap
                       .take(10)
                       .map { case (key, adjusted) =>
                         key + ">" +
                           adjusted.mkString("[", ",", "]") + "=" +
                           adjusted.size
                            }
                       .mkString("{", "\n", "}")
                   )
            println(
                     "'edgesMap.size' is:" +
                       edgesMap.size)
            println(
                     "first 10 'edgesMap's elements are:\n" +
                       edgesMap
                       .take(10)
                       .map { case (Edge(start, end), wheight) =>
                         start + ">" + end + "=" + wheight
                            }
                       .mkString("{", ",", "}")
                   )

            assume(
                    setsMap.size == expectedInputSize,
                    s"must be equal"
                  )
          }
  test(
        "2: 'DijkstraWithTreeMap'" +
          "should " +
          "return right shortest paths "
      ) {
          val destinationsFromFirstOne:
          List[Int] =
            List(7, 37, 59, 82, 99, 115, 133, 165, 188, 197)
          val answerSample: String =
            "1000,1000,1000,1000,1000,2000,1000,1000,1000,1000"

          val expectedShortestPathMap:
          Map[Int, WeightedEdgeWithDistance] =
            Map(
                 0 -> WeightedEdgeWithDistance(
                                                None,
                                                0
                                              ),
                 2 -> WeightedEdgeWithDistance(
                                                 Some(
                                                       WeightedEdge(
                                                                     0,
                                                                     2,
                                                                     26
                                                                   )
                                                     ),
                                                 26
                                               ),
                 4 -> WeightedEdgeWithDistance(
                                                 Some(
                                                       WeightedEdge(
                                                                     0,
                                                                     4,
                                                                     38
                                                                   )
                                                     ),
                                                 38
                                               ),
                 7 -> WeightedEdgeWithDistance(
                                                 Some(
                                                       WeightedEdge(
                                                                     2,
                                                                     7,
34
                                                                   )
                                                     ),
                                                 60
                                               ),
                 5 -> WeightedEdgeWithDistance(
                                                 Some(
                                                       WeightedEdge(
                                                                     4,
                                                                     5,
                                                                     35
                                                                   )
                                                     ),
                                                 73
                                               ),
                 3 -> WeightedEdgeWithDistance(
                                                 Some(
                                                       WeightedEdge(
                                                                     7,
                                                                     3,
                                                                     39
                                                                   )
                                                     ),
                                                 99
                                               ),
                 1 -> WeightedEdgeWithDistance(
                                                 Some(
                                                       WeightedEdge(
                                                                     5,
                                                                     1,
                                                                     32
                                                                   )
                                                     ),
                                                 105
                                               ),
                 6 -> WeightedEdgeWithDistance(
                                                 Some(
                                                       WeightedEdge(
                                                                     3,
                                                                     6,
                                                                     52
                                                                   )
                                                     ),
                                                 151
                                               )
               )
                val expectedDistances:
                List[Int] =
                  expectedShortestPathMap
    //.mapValues{case WeightedEdgeWithDistance(_, d) => d}
    //.toMap
    .keys
    .toList
    .sorted
    .map(k=>expectedShortestPathMap(k).distance)
          val inputTakeNumber: Int =
            3
          val inPutFilePath: String =
          //"/home/gluk-alex/Documents/sbt_projects/"
            "/media/gluk-alex/" +
              "GDI/Java/Scala/sbt/projects/" +
              "stanfordAlgorithmsDesignAndAnalysis1/" +
              "src/test/scala/" +
              //"testSCC/"
              //"testMedianMaintenance/"
              //"testTwoSumAlgorithm/"
              "testShortestPathDijkstra/"
          val inPutFileName: String =
            //"dijkstraData.txt"
    "tinyEWD.txt"
          lazy val (fileContentIter, fileContentIterCopy):
          (Iterator[String], Iterator[String]) =
          //Iterator.empty
            readFromFile(
                          fileName = inPutFileName,
                          filePath = inPutFilePath
                        )
            /*reduce / control input size*/
            //.take(inputTakeNumber)
              /*nodes & edges size*/
            .drop(2)
            .duplicate
          val expectedInputSize: Int =
            3
          val (setsMap,
          edgesMap):
          (collection.mutable.
          Map[Int, Set[Int]],
            collection.mutable.
            Map[Edge, Int]) =
            makeWeightedEdgesAndSetsMapsFromNodesWithAdjusted(
                                                               fileContentIter =
                                                                 fileContentIter
                                                             )
          /*println(
                   "first 10 'setsMap's elements are:\n" +
                     setsMap
                       .take(10)
                     .map{case (key,adjusted) =>
                       key + ">" +
                         adjusted.mkString("[",",","]") + "=" +
                         adjusted.size}
                     .mkString("{","\n","}")
                 )
          println(
                   "'edgesMap.size' is:" +
                     edgesMap.size)
                     println(
                   "first 10 'edgesMap's elements are:\n" +
                     edgesMap
                       .take(10)
                       .map{case (Edge(start,end),wheight) =>
                       start + ">" + end + "=" + wheight}
                     .mkString("{",",","}")
                 )*/
          val maxValue: Int = 1000000
          val distances:
          collection.immutable.
          Map[Int, Int] =
            DijkstraWithEdgesSetsTreeMapMutable(
                                                 setsMap =
                                                   setsMap,
                                                   //.toMap,
                                                 edgesMap =
                                                   edgesMap,
                                                 sourceNode = 1
                                               ).toMap
          /*!!!Warn!!!preserve order!!!must be exact as in List*/
          //'answer' is:2399,4471,3879,4930,3578,3051,2947,6584,2367,2052
          /*'answer' is:6584,4471,2947,2052,2367,2399,3879,4930,3051,3578*/
          val answer: String =
          /*distances
          .collect {
                     case (k, v) if destinationsFromFirstOne.contains(k) =>
                       v.toString
                   }*/
            //destinationsFromFirstOne
                //.map { case node => distances(node) }
                setsMap
            .keys
            .toList
            .sorted
            .map { case node => distances(node) }
            .mkString(",")
          println(
                   "'answer' is:" +
                     answer)

          assume(
                  distances.nonEmpty &&
                    answer == expectedDistances,
                  s"'distances' must be 'nonEmpty'"
                )
        }

}
