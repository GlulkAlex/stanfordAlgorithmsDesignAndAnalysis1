package testShortestPathDijkstra

import org.scalatest.FunSuite

import java.text.SimpleDateFormat
import java.util.Calendar

import filesIO.FilesIO._
import shortestPathByDijkstra.ShortestPathDijkstra._

/**
 * Created by gluk-alex on 8/29/15.
 */
class ShortestPathDijkstraSuit
  extends FunSuite {
  test(
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
           )

          assume(
                  setsMap.size == expectedInputSize,
                 s"must be equal"
                )
        }

}
