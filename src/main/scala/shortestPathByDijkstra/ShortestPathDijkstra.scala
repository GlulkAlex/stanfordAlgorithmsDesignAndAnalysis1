package shortestPathByDijkstra

/**
 * Created by gluk-alex on 8/29/15.
 */
object ShortestPathDijkstra {
  /*
  In this programming problem
  you will code up
  `Dijkstra's shortest-path` algorithm.
  The file contains
  an adjacency list representation of
  an `undirected` `weighted` `graph`
  with '200' `vertices`
  labeled '1' to '200'.
  Each `row` consists of
  the `node` `tuples` that are
  adjacent to
  that particular `vertex`
  along with
  the `length` of that `edge`.
  For example,
  the '6th' `row` has
  '6' as
  the first entry
  indicating that
  this `row` corresponds to
  the `vertex` labeled '6'.
  The next entry of this row "141,8200" indicates that
  there is
  an `edge` between `vertex` '6' and vertex '141' that has `length` '8200'.
  The rest of the `pairs` of this `row` indicate
  the other `vertices` `adjacent` to `vertex` '6' and
  the `lengths` of the corresponding `edges`.
  /*TODO*/
  Your task is
  to run `Dijkstra's shortest-path` algorithm on this `graph`,
  using '1' (the first vertex) as
  the `source` `vertex`, and
  to compute
  the `shortest-path distances`
  between '1' and every other `vertex` of the `graph`.
  If there is
  no `path` between
  a vertex 'v' and vertex '1',
  we shall define
  the `shortest-path` `distance` between '1' and 'v' to be '1000000' (max value).

  You should
  report
  the `shortest-path` `distances` to
  the following ten vertices, in order:
  '7,37,59,82,99,115,133,165,188,197'.
  You should
  `encode` the `distances` as
  a `comma-separated` string of integers.
  So
  if you find that
  all ten of these `vertices` except '115' are
  at `distance` '1000' away from `vertex` '1' and
  '115' is '2000' `distance` away, then
  your answer should be
  '1000,1000,1000,1000,1000,2000,1000,1000,1000,1000'.
  Remember
  the `order` of reporting !!!DOES MATTER!!!, and
  the string should be
  in the same `order` in which
  the above ten `vertices` are given.
  Please type your answer in the space provided.

  IMPLEMENTATION NOTES:
  This graph is
  small enough that
  the `straightforward` O(mn) time implementation of Dijkstra's algorithm
  should work fine.
  OPTIONAL:
  For those of you seeking an additional challenge,
  try
  implementing the `heap-based` version.
  Note
  this requires
  a `heap` that supports
  `deletions`, and
  you will probably need to
  maintain some kind of
  `mapping` between `vertices` and
  their positions in the `heap`.
   */

  case class Edge(
                   startNode: Int,
                   endNode: Int)

  case class WeightedEdge(
                         /*interchangeable for undirected graph*/
                   startNode: Int,
                   endNode: Int,
                   weight: Int
                           )

  val inputSample1: String =
  "12	78,5753	17,4602	62,5676	16,8068	60,5933	67,371	71,6734	53,7001	72,3626	" +
    "34,6690	59,761	18,1520	128,7542	38,6699	57,9416"

  val inputSample2: String =
  "17\t26,1275\t45,5114\t142,8016\t83,4615\t140,6440\t8,3535\t69,3610\t153," +
    "8545\t9,7002\t12,4602\t173,7312\t114,8915\t108,1942\t54,3115\t66," +
    "6176\t190,7000\t70,3899\t5,2514\t178,7464\t166,4762\t2,5409\t146," +
    "5362\t117,6266\t"

  val edgeSample1: String = "12	17,4602"
  val edgeSample2: String = "17\t12,4602"

  val edgesMapSample:
  Map[Int, WeightedEdge] = Map(
                                12 -> WeightedEdge(12,17,4602),
                                17 -> WeightedEdge(17,12,4602)
                              )

  val allNodesSet: Set[Int]  =
    (1 to 200)
    .toSet

  val allNodesList: List[Int]  =
    (1 to 200)
    .toList

  val allNodesArray: Array[Int]  =
    (1 to 200)
    .toArray

  /*assume that 'Iterator' contains only well formed formatted lines*/
  @scala.annotation.tailrec
  def makeSetsMapFromNodesWithAdjusted(
                                        fileContentIter: Iterator[String],
                                        resultMap:
                                        collection.mutable
                                        .Map[Int,
                                          Set[Int]] =
                                        collection.mutable.Map.empty,
                                        progressCounter: Int = 0
                                        ):
  collection.mutable.
  Map[Int, Set[Int]] = {

    if (fileContentIter.isEmpty) {
      /*side effect*/
      print(" " * 200 + "\r")
      print(s"Total: $progressCounter nodes with adjusted " +
              s"was readied from file\n")
      //println
      /*return value*/
      resultMap
    } else /*if (adjacencyList.hasNext)*/ {
      val nextStr: String =
        fileContentIter
        /*to converge to empty iterator eventually*/
        .next()
      val digitsLine:
      List[Int] =
        nextStr
        .split(",")
        .view
        .map(_.toInt)
        .toList
      /*side effect*/
      if (progressCounter % 500000 == 0) {
        print(s"Current progress: $progressCounter nodes are readied from " +
                s"file\r")
      } else if (progressCounter == 0) {
        print(s"Starting reading nodes from file ...\r")
      }

      //assume(digitsLine.nonEmpty && digitsLine.length >= 1)
      val mapKey: Int =
        digitsLine
        .head
      val mapValue: Set[Int] =
        digitsLine
        .tail
        .toSet
      val resultMapUpdated: collection.mutable.Map[Int, Set[Int]] =
        resultMap
        .updated(
            key = mapKey,
            //adjustedNodes
            value =
              mapValue
                )
      /*recursion*/
      makeSetsMapFromNodesWithAdjusted(
                                        fileContentIter: Iterator[String],
                                        resultMap =
                                          resultMapUpdated,
                                        progressCounter = progressCounter + 1
                                      )
    }
  }

}
