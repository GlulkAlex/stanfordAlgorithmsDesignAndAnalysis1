package shortestPathByDijkstra

import scala.collection.mutable.Map
import scala.collection.mutable.PriorityQueue
import scala.collection.immutable.TreeMap

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
  "12	78,5753	17,4602	62,5676	16,8068	60,5933	67,371	" +
    "71,6734	53,7001	72,3626	" +
    "34,6690	59,761	18,1520	128,7542	38,6699	57,9416"

  val inputSample2: String =
  "17\t26,1275\t45,5114\t142,8016\t83,4615\t140,6440\t8,3535\t69,3610\t153," +
    "8545\t9,7002\t12,4602\t173,7312\t114,8915\t108,1942\t54,3115\t66," +
    "6176\t190,7000\t70,3899\t5,2514\t178,7464\t166,4762\t2,5409\t146," +
    "5362\t117,6266\t"

  val edgeSample1: String = "12	17,4602"
  val edgeSample2: String = "17\t12,4602"

  val edgesMapSample1:
  Map[Int, WeightedEdge] = Map(
                                12 -> WeightedEdge(12,17,4602),
                                17 -> WeightedEdge(17,12,4602)
                              )
  val edgesMapSample2:
  Map[String, Int] = Map(
                                "12>17" -> 4602
                              )
  val edgesMapSample3:
  Map[Edge, Int] = Map(
                        Edge(12,17) -> 4602
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
  def makeWeightedEdgesAndSetsMapsFromNodesWithAdjusted(
                                        fileContentIter: Iterator[String],
                                        resultSetsMap:
                                        collection.mutable.Map[Int,
                                          Set[Int]] =
                                        collection.mutable.Map.empty,
                                        resultEdgesMap:
                                        collection.mutable.Map[Edge,
                                          Int] =
                                        collection.mutable.Map.empty,
                                        progressCounter: Int = 0
                                        ):
  //collection.mutable.
  //Map[Int, Set[Int]] = {
  (collection.mutable.Map[Int, Set[Int]],
    collection.mutable.Map[Edge, Int]) = {

      if (fileContentIter.isEmpty) {
        /*side effect*/
        print(" " * 200 + "\r")
        print(s"Total: $progressCounter nodes with adjusted " +
                s"was readied from file\n")
        //println
        /*return value*/
        (resultSetsMap, resultEdgesMap)
      } else /*if (adjacencyList.hasNext)*/ {
        val nextStr: String =
          fileContentIter
          /*to converge to empty iterator eventually*/
          .next()
        val digitsLine:
        List[String] =
        //List[Int] =
          nextStr
          .split("\t")
          //.split(",")
          //.view
          //.map(_.toInt)
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
          .toInt
        /*val mapValue: Set[Int] =
          digitsLine
          .tail
          .toSet*/
        var nodeEdgesMap: Map[Edge,Int] = Map.empty
        var mapValue:
        //collection.mutable.
        Set[Int] =
          //collection.mutable.
          Set.empty
        /*side effect*/
        digitsLine
        .tail
        .foreach {
                   str =>
                     //val nodeWithWheight:
                     val List(node, wheight):
                     List[Int] =
                       str
                       .split(",")
                       .map(_.toInt)
                       .toList
                     /*side effect*/
                     mapValue += node
                     /*side effect*/
                     if (
                       nodeEdgesMap
                       .contains(Edge(mapKey, node)) ||
                         nodeEdgesMap
                         .contains(Edge(node, mapKey))
                     ) {
                       //skip, has already
                     } else {
                       nodeEdgesMap =
                         nodeEdgesMap
                         //Same as ms + (k -> v)
                         .updated(Edge(mapKey, node), wheight)
                     }
                 }
        /*val resultMapUpdated:
        collection.mutable.
        Map[Int, Set[Int]] =
          resultSetsMap
          .updated(
              key = mapKey,
              //adjustedNodes
              value =
                mapValue
                  )*/
        /*side effect*/
        resultSetsMap += (mapKey -> mapValue)
        /*side effect*/
        nodeEdgesMap
        .foreach{
                  case (edgeKey, edgeWeight) =>
                  //edge =>
          /*?only minimal edges should remain?*/
                    val previousEdgeVal: Option[Int] =
                      resultEdgesMap
                      .get(edgeKey)
                    /*?what about reversed edge?*/
                    val previousEdgeReversedVal: Option[Int] =
                      resultEdgesMap
                      .get(Edge(edgeKey.endNode, edgeKey.startNode))

          if (
            /*resultEdgesMap
            .contains(edgeKey)*/
            //.contains(edge._1)
            previousEdgeVal.nonEmpty
          ) {
            if (previousEdgeVal.get > edgeWeight) {
              /*side effect*/
              resultEdgesMap +=
                (edgeKey -> edgeWeight)
            } else {
              //skip
            }
          } else if (
                             previousEdgeReversedVal.nonEmpty
          ) {
            if (previousEdgeReversedVal.get > edgeWeight) {
              /*side effect*/
              resultEdgesMap +=
                (Edge(edgeKey.endNode, edgeKey.startNode) -> edgeWeight)
            } else {
              //skip
            }
          } else {
            /*side effect*/
            resultEdgesMap +=
              (edgeKey -> edgeWeight)
              //(edge._1 -> mapValue)
          }
                }
        /*recursion*/
        makeWeightedEdgesAndSetsMapsFromNodesWithAdjusted(
                                                           fileContentIter:
                                                             Iterator[String],
                                                           resultSetsMap =
                                                             resultSetsMap,
                                                             //resultMapUpdated,
        resultEdgesMap =
                                                             resultEdgesMap,
                                                           progressCounter =
                                                             progressCounter + 1
                                                         )
      }
  }

  /*
  algorithm:
  Let the `node`
  at which we are `starting`
  be called the `initial` `node`.
  Let the `distance` of `node` 'Y' be
  the `distance` from
  the `initial` `node` to 'Y'.
  Dijkstra's algorithm will
  assign some `initial` `distance` values and
  will try to
  `improve` them step by step.
  >1>Assign to every `node`
  a `tentative`(preliminary) `distance` value:
  set it to `zero`
  for our `initial` `node` and
  to `infinity`
  for all other nodes.
  >2>Set the `initial` `node` as `current`.
  Mark all other `nodes` `unvisited`.
  Create
  a set of
  all the `unvisited` `nodes` called the `unvisited set`.
  >3>For the `current` `node`,
  consider
  all of its `unvisited` `neighbors` and
  calculate their `tentative` `distances`.
  Compare
  the newly calculated `tentative` `distance` to
  the `current` assigned value and
  assign the smaller one.
  For example,
  if the `current` `node` 'A' is
  marked with a `distance` of '6', and
  the `edge` connecting it with a `neighbor` 'B' has `length` '2', then
  the `distance` to 'B' (through 'A') will be
  '6 + 2 = 8'.
  If 'B' was
  previously marked with
  a `distance` greater than '8' then
  change it to '8'.
  Otherwise,
  keep the current value.
  >4>When we are done considering
  all of the `neighbors` of the `current` `node`,
  mark the `current` `node` as `visited` and
  remove it from the `unvisited set`.
  A `visited` `node` will never be checked again.
  >5>If
  the `destination` `node` has been marked `visited`
  (when planning a `route` between two specific `nodes`) or
  if
  the smallest `tentative` `distance` among the `nodes` in
  the `unvisited` set is
  `infinity` (when planning a `complete traversal`;
  occurs when
  there is
  `no connection` between
  the `initial` `node` and
  remaining `unvisited` `nodes`), then stop.
  The algorithm has finished.
  >6>Otherwise,
  select the `unvisited` `node` that is
  marked with
  the `smallest` `tentative` `distance`,
  set it as
  the new "current node", and
  go back to step '3'.
   */
  /*
  In the following algorithm,
  the code 'u ← vertex' in 'Q' with 'min dist[u]',
  searches for
  the `vertex` 'u' in the `vertex` set 'Q'
  that has the least 'dist[u]' value.
  'length(u, v)' returns
  the `length` of the `edge` joining (i.e. the distance between)
  the two neighbor-nodes (adjusted) 'u' and 'v'.
  The variable 'alt' on line 19 is
  the `length` of the `path` from
  the `root` `node` to the neighbor `node` 'v'
  if it were to go through 'u'.
  If this `path` is
  shorter than the `current` `shortest path` recorded for 'v',
  that `current path` is
  replaced with this 'alt' `path`.
  The 'prev' array is populated with
  a pointer to the "next-hop" `node` on the source `graph` to
  get the shortest `route` to the `source`.
  1  function Dijkstra(Graph, source):
  2
  3      dist[source] ← 0                       // Distance from source to source
  4      prev[source] ← undefined               // Previous node in optimal path initialization
  5
  6      create vertex set Q
  7
  8      for each vertex v in Graph:             // Initialization
  9          if v ≠ source:                      // v has not yet been removed from Q (unvisited nodes)
  10              dist[v] ← INFINITY             // Unknown distance from source to v
  11              prev[v] ← UNDEFINED            // Previous node in optimal path from source
  12          add v to Q                          // All nodes initially in Q (unvisited nodes)
  13
  14      while Q is not empty:
  15          u ← vertex in Q with min dist[u]    // Source node in the first case
  16          remove u from Q
  17
  18          for each neighbor v of u:           // where v is still in Q.
  19              alt ← dist[u] + length(u, v)
  20              if alt < dist[v]:               // A shorter path to v has been found
  21                  dist[v] ← alt
  22                  prev[v] ← u
  23
  24      return dist[], prev[]
  */
  /*
  If we are
  only interested in
  a `shortest path` between `vertices` `source` and `target`,
  we can
  terminate the search after line 16
  if 'u = target'.
  Now we can
  read the `shortest path` from `source` to `target` by
  reverse iteration:
  1  S ← empty sequence
    2  u ← target
  3  while prev[u] is defined:                   // Construct the shortest path with a stack S
  4      insert u at the beginning of S          // Push the vertex onto the stack
    5      u ← prev[u]                            // Traverse from target to source
  Now sequence 'S' is
  the list of `vertices` constituting
  one of the `shortest paths` from `source` to `target`, or
  the empty sequence if no path exists.
  A more general problem would be
  to find all the `shortest paths` between `source` and `target`
  (there might be several different ones of the same length).
  Then
  instead of
  storing only a single `node` in each entry of 'prev[]'
  we would
  store all `nodes` satisfying the `relaxation condition`.
  For example,
  if both 'r' and `source` connect to `target` and
  both of them lie on different `shortest paths`
  through `target`
  (because the `edge` cost is the same in both cases),
  then
  we would
  add both 'r' and `source` to 'prev[target]'.
  When the algorithm completes,
  'prev[]' data structure will actually describe
  a `graph` that is
  a subset of the original `graph` with some `edges` removed.
  Its key property will be
  that
  if the algorithm was run with some `starting` `node`,
  then
  every `path` from that node to
  any other `node` in the new `graph` will be
  the `shortest path` between those `nodes` in the original `graph`, and
  all `paths` of that `length` from the original `graph` will be
  present in the new `graph`.
  Then
  to actually find all these `shortest paths`
  between two given nodes we would use
  a `path finding` algorithm on the new `graph`, such
  as `depth-first search`.
  */

  /*
  Using a priority queue
  A `min-priority` queue is
  an abstract data type
  that
  provides 3 basic operations:
  'add_with_priority()',
  'decrease_priority()' and
  'extract_min()'.
  As mentioned earlier,
  using such a `data structure` can lead to
  faster computing times than using a basic `queue`.
  Notably,
  `Fibonacci heap` (Fredman & Tarjan 1984) or
  `Brodal queue`
  offer optimal implementations for those 3 operations.
  As the algorithm is slightly different,
  we mention it here, in pseudo-code as well :
  1  function Dijkstra(Graph, source):
  2      dist[source] ← 0 // Initialization
  3
  4      create vertex set Q
  5
  6      for each vertex v in Graph:
  7          if v ≠ source
  8              dist[v] ← INFINITY // Unknown distance from source to v
  9              prev[v] ← UNDEFINED // Predecessor of v
  10
  11         Q.add_with_priority(v, dist[v])
  12
  13
  14      while Q is not empty: // The main loop
  15         u ← Q.extract_min() // Remove and return best vertex
  16         for each neighbor v of u: // only v that is still in Q
  17             alt = dist[u] + length(u, v)
  18             if alt < dist[v]
  19                 dist[v] ← alt
  20                 prev[v] ← u
  21                 Q.decrease_priority(v, alt)
  22
  23     return dist[], prev[]//?stored path?from source to sink?
  Instead of
  filling the `priority queue` with
  all nodes in the `initialization phase`,
  it is also possible to
  initialize it to contain only `source`;
  then,
  inside the 'if alt < dist[v]' block,
  the `node` must be inserted
  if not already in the `queue`
  (instead of
  performing a `decrease_priority` (?update previous value?) operation).
  Other `data structures` can be used to
  achieve even faster computing times in practice.
  */

  /*return all graph `nodes` with shortest `distances` to 'source'
  where 'Int.MaxValue' stands for unreachable from 'source'
  * */
  def DijkstraWithTreeMap(
                           //graph,
                           /*read only*/
                           setsMap:
                           collection.immutable.
                           Map[Int, Set[Int]],
                           /*read only*/
                           edgesMap:
                           Map[Edge, Int],
                           distances:
                           collection.immutable.
                           Map[Int, Int] =
                           collection.immutable.
                           Map.empty,
                           /*start node*/
                           sourceNode: Int = 1
                           ):
  collection.immutable.
  Map[Int, Int] = {
    /*initialization*/
    val currentDistances:
    collection.immutable.
    Map[Int, Int] =
    if (distances.isEmpty) {
      setsMap
      //.mapValues{nodeValue => Int.MaxValue} +
      .mapValues(_ => Int.MaxValue) +
        (sourceNode -> 0)
      //.toMap
    } else {
      distances
    }
    val explored: Set[Int] = Set.empty
    val unExplored: Set[Int] =
      setsMap
    .keySet
    .toSet
    /*will be reduced to empty eventually*/
    val nodesIterator: Iterator[(Int, Set[Int])] =
      (setsMap - sourceNode)
      .iterator
    /*val priorityQueueOrdering =
      Ordering.fromLessThan[String](_ > _)*/
    val minDistancePriorityQueue: TreeMap[Int, Int] =
      scala.collection.immutable.TreeMap
        .empty(Ordering[Int].reverse) +
        /*set 'sourceNode' as start*/
      (sourceNode -> 0)

    @scala.annotation.tailrec
    def checkAdjusted(
                       /*read only*/
                       sourceNodeToCheck: Int,
                       /*?read only?*/
                       sourceDistanceToCheck: Int,
                       adjustedIterToCheck: Iterator[Int],
                       distancesToCheck:
                       collection.immutable.
                       Map[Int, Int],
                       priorityQueueToCheck: TreeMap[Int, Int]
                       ):
    (
      collection.immutable.
      Map[Int, Int], TreeMap[Int, Int]) = {
      if (adjustedIterToCheck.isEmpty) {
      /*return value*/
        (distancesToCheck, priorityQueueToCheck)
    } else {
        val candidateNodeKey: Int =
          adjustedIterToCheck
          /*to converge to empty eventually*/
          .next()
        //assume(distancesToCheck.isDefinedAt(candidateNodeKey))
        val previousNodeDistance: Int =
          distancesToCheck(candidateNodeKey)
        //assume that at least one is defined
        val edgeWeight: Int =
          /*if (edgesMap.contains(Edge(sourceNodeToCheck,candidateNodeKey))) {
            edgesMap(Edge(sourceNodeToCheck, candidateNodeKey))
          } else {
            edgesMap(Edge(candidateNodeKey, sourceNodeToCheck))
          }*/
          edgesMap
        .getOrElse(
              Edge(sourceNodeToCheck, candidateNodeKey),
              edgesMap(Edge(candidateNodeKey, sourceNodeToCheck))
                  )
        val candidateDistances: Int =
          sourceDistanceToCheck + edgeWeight
        val (distancesUpdated, priorityQueueUpdated) =
        if (candidateDistances < previousNodeDistance) {
          (
            distancesToCheck +
            (candidateNodeKey->candidateDistances),
            if (priorityQueueToCheck.contains(candidateNodeKey)) {
              priorityQueueToCheck
            } else {
              priorityQueueToCheck + (candidateNodeKey->candidateDistances)
            }
            )
        } else {
          (distancesToCheck, priorityQueueToCheck)
        }
        /*recursion*/
        checkAdjusted(
                       sourceNodeToCheck = sourceNodeToCheck,
                       sourceDistanceToCheck = sourceDistanceToCheck,
                       adjustedIterToCheck = adjustedIterToCheck,
                       distancesToCheck = distancesUpdated,
                       priorityQueueToCheck = priorityQueueUpdated
                     )
      }
    }

    @scala.annotation.tailrec
    def innerLoop(
                   innerDistances:
                   collection.immutable.
                   Map[Int, Int],
                   //innerExplored: Set[Int],
                   //innerUnExplored: Set[Int],
                   //currentSourceNode: Int,
                   innerPriorityQueue: TreeMap[Int, Int]
                   ):
    collection.immutable.
    Map[Int, Int] = {
      if (innerPriorityQueue.isEmpty) {
        /*return value*/
        innerDistances
      } else {
        val (currentSourceKey, currentSourceDistance): (Int, Int) =
          innerPriorityQueue
          .head
          //.min
          //.invert
        assume(setsMap.nonEmpty,"'setsMap' must be '.nonEmpty'")
        val adjustedIter: Iterator[Int] =
            setsMap(currentSourceKey)
              //.get(currentSourceKey)
              //.get
        .iterator
        val (distancesUpdated, priorityQueueUpdated):
          (collection.immutable.
            Map[Int, Int], TreeMap[Int, Int]) =
            checkAdjusted(
                           sourceNodeToCheck =
                             currentSourceKey,
                           sourceDistanceToCheck =
                             currentSourceDistance,
                           adjustedIterToCheck = adjustedIter,
                           distancesToCheck =
                             innerDistances,
                           priorityQueueToCheck =
                             innerPriorityQueue
                             .tail
                         )
        /*recursion*/
        innerLoop(
                   innerDistances =
                     distancesUpdated,
                   //innerExplored: Set[Int],
                   //innerUnExplored: Set[Int],
                   //currentSourceNode: Int,
                   innerPriorityQueue =
                     priorityQueueUpdated
                 )
      }
    }
    /*initialization*/
    innerLoop(
               innerDistances = currentDistances,
               //innerExplored = explored,
               //innerUnExplored = unExplored,
               //currentSourceNode = sourceNode,
               innerPriorityQueue = minDistancePriorityQueue
             )
  }

  def UniformCostSearch(
                         graph:
                         Map[Int,
                          Set[WeightedEdge]],
                         /*source vertex*/
                         start: Int = 1,
                         goal: Int): Int = {
    var currentNode: Int = start
    var currentNodeVal: Set[WeightedEdge] =
      graph.get(start).get
    var cost: Int = 0
    /*val heapMin = new
        mutable.PriorityQueue()(
                                 Ordering[Int].reverse
                               )*/
    //priority queue containing node only
    var frontier =
      new
          collection.mutable.PriorityQueue()(
                                   /*Ordering[(Int, WeightedEdge)]
                                   .on((x) => x)*/
                                   Ordering
                                   .by[WeightedEdge, Int](_.weight)
                                   .reverse
                                 )
    var explored: Set[Int] = Set.empty[Int]

    /*do
      if (frontier.isEmpty) {
        //return failure
      } else {

      }*/
    while (frontier.nonEmpty) {
      currentNode = frontier
                    //.pop()
                    .dequeue().startNode
      if (
      //node is goal
        currentNode == goal
      ) {
        //return solution
        cost
      }
      //.add(node)
      explored += currentNode

      /*side effect*/
      for {
        //each of node's neighbors n
        node<-currentNodeVal
      } {
        if (
          //n is not in explored
          !explored
           .contains(
               node
               .endNode
                    )
        ) {
          if (
            //n is not in frontier
            /*!frontier
             .exist(
                 _ == node
                      )*/
            frontier
             .forall(
                 _ != node
                   )
          ) {
            //.add(n)
            frontier += node
          }  else {
            if (
              //n is in frontier with higher cost
              node.weight >
                frontier.find(_.startNode == currentNode).get.weight
            )
            {
              //replace existing node with n
              /*? in the 'frontier'? How ?*/
            } else {
              //skip?
            }
          }
        } else {
          //skip
        }
      }
    }
    cost
  }//end def

}
