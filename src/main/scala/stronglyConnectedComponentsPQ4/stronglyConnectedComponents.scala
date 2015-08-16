package stronglyConnectedComponentsPQ4

import scala.collection.BitSet
import scala.math.log
import scala.math.min
import scala.math.max
import scala.collection.immutable.Queue
import scala.collection.immutable.Stack
import scala.collection.mutable.ArrayBuffer
import randomGenerators.RandomGenerators.randomIntWithinInterval

import scala.util.matching.Regex

/**
 * Created by gluk-alex on 7/30/15.
 */
object stronglyConnectedComponents {

  /*
  Programming Question-4
  >The due date for this homework is Sun 9 Aug 2015 11:59 PM PDT.

  Question 1
  Download the text file 'SCC.txt'.
  The file contains
  the `edges` of a `directed` `graph`.
  `Vertices` are
  labeled as
  positive integers from '1 to 875714'.
  Every `row` indicates
  an `edge`,
  the `vertex` `label` in first `column` is
  the `tail` and
  the `vertex` `label` in second `column` is
  the `head`
  (recall the `graph` is `directed`, and
  the `edges` are `directed` from
  the first `column` `vertex` to
  the second `column` `vertex`).
  So
  for example,
  the '11-th' `row` looks likes: "2 47646".
  This just means that
  the `vertex` with `label` '2' has
  an outgoing `edge` to
  the `vertex` with label '47646'
  TODO
  Your task is
  to code up
  the algorithm from
  the video lectures
  for computing `strongly connected components` (SCCs), and
  to run this algorithm on the given `graph`.

  Output Format:
  You should output the `sizes` of
  the '5' largest `SCCs` in the given `graph`,
  in `decreasing` order of `sizes`,
  separated by `commas` (!!!avoid any `spaces`!!!).
  So
  if your algorithm computes
  the `sizes` of
  the 'five' largest `SCCs` to be
  '500, 400, 300, 200 and 100', then
  your answer should be
  "500,400,300,200,100".
  If your algorithm finds
  less than '5' `SCCs`,
  then
  write '0' for the remaining terms.
  Thus,
  if your algorithm computes only '3' `SCCs` whose sizes are
  '400, 300, and 100', then
  your answer should be
  "400,300,100,0,0".

  WARNING:
  This is the most challenging programming assignment of the course.
  Because of the `size` of the graph
  you may have to
  manage `memory` carefully.
  The best way to do this
  depends on your programming language and
  environment, and
  we strongly suggest
  that
  you exchange tips for doing this on the discussion forums.
   */

  /*
  `Bitsets` are
  hence more compact than other `sets`
  if
  they contain many small elements.
  Another advantage of `bitsets` is that
  operations such as
  `membership` 'test' with 'contains', or
  element `addition` and
  `removal`
  with '+=' and
  '-=' are
  all `extremely efficient`.
  ---
  So,
  may be use it as 'isExplored' check ?
   */
  trait TraverseDirection

  case object Up extends TraverseDirection

  case object Down extends TraverseDirection

  case class StackLeader(
                          leaderKey: Int,
                          nodeKey: Int
                          //adjustedNodeKey: Int
                          )

  case class ComponentResult(
                              nodesAmount: Int,
                              exploredNodes: Vector[Int])

  /*node's 'rank' or 'layer'*/
  case class IsExploredNode(
                             node: Int,
                             var isExplored: Boolean
                             ) {
    override def toString =
      s"""$node[${if (isExplored) "e" else "u"}]"""
  }

  case class DFSResults(
                         preOrder:
                         List[Int],
                         //Stream[Int],
                         postOrder:
                         List[Int],
                         //Stream[Int],
                         reachable: Int
                         )

  case class DFSResultInt(
                           preOrder:
                           List[Int],
                           //Stream[Int],
                           postOrder:
                           List[Int]
                           //Stream[Int]
                           )

  case class DepthFirstSearchResult(
                                     /*preOrder: List[IsExploredNode],
                                     postOrder: List[IsExploredNode]*/
                                     preOrder: Stream[IsExploredNode],
                                     postOrder: Stream[IsExploredNode]
                                     )

  /*node's 'rank' or 'layer'*/
  case class IndexedNode(
                          /*or 'nodeKey' in Map*/
                          nodeVal: Int,
                          var nodeIndex: Option[Int],
                          var nodeLowLink: Int,
                          /*or check Set.contains(nodeKey)*/
                          var isInStack: Boolean
                          )

  case class NodeMapValFieldsStatic(
                                     isExplored: Boolean,
                                     //nodeIndex: Option[Int],
                                     /*default is 'Int.MaxValue'*/
                                     //nodeLowLink: Int,
                                     //inStack: Boolean,
                                     doneOrder: Option[Int],
                                     //var groupLeader: Int
                                     adjustedNodes: Set[Int]
                                     ) {
    override def toString =
      s"""[${if (isExplored) "e" else "u"}|$doneOrder(${
        adjustedNodes.mkString(",")
      })]"""
  }

  /**
  @note Helper class
  @author Gluk Alex
  @version 1.1, 13/08/2015
  @define bfinfo copy paste from `Option`
  @param nodeIndex: Option[Int]
  @param nodeLowLink: Int
  @param adjustedNodes: Set[Int]
  @return represent corresponding to node key value in th Map with fields
    */
  case class NodeMapValFieldsDynamic(
                                      /*disable all redundant*/
                                      //var isExplored: Boolean,
                                      var nodeIndex:
                                      /*replace with value out of bound (-1)*/
                                      Int,
                                      /*change option */
                                      //Option[Int],
                                      /*default is 'Int.MaxValue'*/
                                      var nodeLowLink: Int,
                                      //var isInStack: Boolean,
                                      //var doneOrder: Option[Int],
                                      //var groupLeader: Option[Int],
                                      var adjustedNodes:
                                      Stream[Int]
                                      //Set[Int]
                                      ) {
    override def toString =
    //s"""{${if (isExplored) "e" else "u"}/""" +
      s"""i=$nodeIndex,""" +
        s"""l=${
          if (nodeLowLink == Int.MaxValue) {
            "maxInt"
          } else {
            nodeLowLink.toString
          }
        }""" +
        //s"""${if (isInStack) "in" else "out"}/""" +
        //s"""$doneOrder/""" +
        //s"""$groupLeader>to(""" +
        s""",{${adjustedNodes.mkString(",")}}"""
  }

  case class NodeFieldsArray(
                              var nodeKey: Int,
                              /*compound value fields*/
                              var nodeIndex:
                              /*replace with value out of bound like (-1)*/
                              Int,
                              var nodeLowLink: Int,
                              var adjustedNodes:
                              Stream[Int]
                              ) {
    override def toString =
      s"""key($nodeKey)""" +
        s""",i:$nodeIndex""" +
        s",l:" + {
        if (nodeLowLink == Int.MaxValue) {
          "maxInt"
        } else {
          nodeLowLink.toString
        }
      } +
        s""",{${adjustedNodes.mkString(",")}}"""
  }


  case class RankedNode(node: Int, rank: Int)

  case class Edge(
                   startNode: Int,
                   endNode: Int)

  /*directed edge*/
  case class ArcFromNodes(
                           arcTail: IsExploredNode,
                           arcHead: IsExploredNode) {
    override def toString =
      s"""{${arcTail.node}[${if (arcTail.isExplored) "e" else "u"}]->""" +
        s"""${arcHead.node}[${if (arcHead.isExplored) "e" else "u"}]}"""

    //final def showNode: String = this.toString
  }

  /*directed edge*/
  case class Arc(
                  arcTail: Int,
                  arcHead: Int) {
    override def toString = s">$arcTail-->$arcHead>"
  }

  case class UnDirectedGraph(
                              nodes: Vector[Int],
                              edges: Vector[Edge])

  case class DirectedGraph(
                            //nodes: Vector[Int],
                            nodes: Seq[Int],
                            arcs: Vector[Arc])

  case class DirectedRankedGraph(
                                  nodes: Vector[RankedNode],
                                  arcs: Vector[Arc])

  /*to reduce search time of induced arcs*/
  case class AdjacencyListElem(
                                node: IsExploredNode,
                                /*'prepend', 'head', 'tail' must be enough for
                                create &
                                traversal*/
                                adjustedNodes: List[Int]
                                ) {
    override def toString =
      s"""$node>to>${adjustedNodes.mkString("", ">", "")}"""
  }

  case class IndexedNodeWithAdjacencyList(
                                           node: IndexedNode,
                                           adjustedNodes: List[IndexedNode]
                                           )

  case class ExplorableNodeWithAdjusted(
                                         node: IsExploredNode,
                                         //adjustedNodes: List[IsExploredNode]
                                         adjustedNodes: Stream[IsExploredNode]
                                         )

  case class SCC_Result(
                         lowlink: Int,
                         index: Int,
                         stack: List[IndexedNode],
                         sCC: List[Int]
                         )

  /*instead of 'indexOf'*/
  //@scala.annotation.tailrec
  def randomSearchForArcTailIndex(
                                   arcs: Vector[ArcFromNodes],
                                   tailValue: Int,
                                   startIndex: Int = 0,
                                   endIndex: Int = 0
                                   ): Option[Int] = {
    None
  }

  trait Graph {
    //var nodesWithAdjusted: Map[Int, Any] = Map.empty

    def addEdge(edgeStartNodeKey: Int, edgeEndNodeKey: Int):
    Unit

    //Map[Int, Any]
  }

  class DiGraphSetsMap extends Graph {
    /*override*/ var nodesWithAdjusted: Map[Int, Set[Int]] =
      Map.empty

    /*may be 'Unit' and not have return value*/
    def addEdge(
                 arcTail: Int,
                 arcHead: Int
               /*retun type must concord declared in trait*/
                 //): Map[Int, Set[Int]] = {
      ): Unit = {
      val arcTailGetAdjusted: Option[Set[Int]] =
        nodesWithAdjusted
        .get(key = arcTail)
      val arcHeadGetAdjusted: Option[Set[Int]] =
        nodesWithAdjusted
        .get(key = arcHead)
      //val addArcHead: Map[Int, Set[Int]] =
        if (arcHeadGetAdjusted.isEmpty) {
          /*add new 'node' / first occurrence*/
          /*side effect*/
          //nodesWithAdjusted =
          nodesWithAdjusted
          .updated(
              key = arcHead,
              value = Set.empty
                  )
          /*return value*/
          //nodesWithAdjusted
        } else /*if (arcTailGetAdjusted.isDefined)*/ {
          /*same value*/
          nodesWithAdjusted
        }
      //val resultMapUpdated: Map[Int, Set[Int]] =
        if (arcTailGetAdjusted.isEmpty) {
          /*add new 'node' / first occurrence*/
          /*side effect*/
          nodesWithAdjusted =
          nodesWithAdjusted
          //addArcHead
          .updated(
              key = arcTail,
              value = //valToAdd
                Set(arcHead)
                  )
          /*return value*/
          //nodesWithAdjusted
        } else /*if (arcTailGetAdjusted.isDefined)*/ {
          /*add new adjusted 'node' to existed 'node'*/
          /*side effect*/
          nodesWithAdjusted =
          nodesWithAdjusted
          //addArcHead
          .updated(
              key = arcTail,
              value = //valUpdated
                arcTailGetAdjusted
                .get + arcHead
                  )
          /*return value*/
          //nodesWithAdjusted
        }
      /*return value*/
      //resultMapUpdated
    }
  }

  class DirectedGraphDynamic extends Graph {
    /*override*/ var nodesWithAdjusted: Map[Int, NodeMapValFieldsDynamic] =
      Map.empty

    /*may be 'Unit' and not have return value*/
    def addEdge(
                 arcTail: Int,
                 arcHead: Int
                 //): Map[Int, NodeMapValFieldsDynamic] = {
                 ): Unit = {
      val defaultMapValue: NodeMapValFieldsDynamic =
        NodeMapValFieldsDynamic(
                                 //isExplored = false,
                                 nodeIndex =
                                   -1,
                                 //None,
                                 nodeLowLink = Int.MaxValue,
                                 //isInStack = false,
                                 //doneOrder = None,
                                 //groupLeader = None,
                                 adjustedNodes =
                                   //Set
                                   Stream
                                   .empty
                               )
      val arcTailGet: Option[NodeMapValFieldsDynamic] =
        nodesWithAdjusted
        .get(key = arcTail)
      val arcHeadGet: Option[NodeMapValFieldsDynamic] =
        nodesWithAdjusted
        .get(key = arcHead)
      //val addArcHead: Map[Int, NodeMapValFieldsDynamic] =
      if (arcHeadGet.isEmpty) {
        /*add new 'node' / first occurrence*/
        /*side effect*/
        nodesWithAdjusted =
          nodesWithAdjusted
          .updated(
              key = arcHead,
              value = defaultMapValue
                  )
        /*return value*/
        //nodesWithAdjusted
      } else /*if (arcTailGet.isDefined)*/ {
        /*same value*/
        //nodesWithAdjusted
      }
      //val resultMapUpdated: Map[Int, NodeMapValFieldsDynamic] =
      if (arcTailGet.isEmpty) {
        /*add new 'node' / first occurrence*/
        /*val valToAdd: NodeMapValFieldsDynamic = {
          /*side effect*/
          defaultMapValue.adjustedNodes = Set(arcHead)
          /*return value*/
          defaultMapValue
        }*/
        /*side effect*/
        nodesWithAdjusted =
          //addArcHead
          nodesWithAdjusted
          .updated(
              key = arcTail,
              value = //valToAdd
                NodeMapValFieldsDynamic(
                                         //isExplored = false,
                                         nodeIndex =
                                           -1,
                                         //None,
                                         nodeLowLink = Int.MaxValue,
                                         //isInStack = false,
                                         //doneOrder = None,
                                         //groupLeader = None,
                                         adjustedNodes =
                                           Stream(arcHead)
                                         //Set(arcHead)
                                       )
                  )
        /*return value*/
        //nodesWithAdjusted
      } else /*if (arcTailGet.isDefined)*/ {
        /*add new adjusted 'node' to existed 'node'*/
        /*val valUpdated: NodeMapValFieldsDynamic = {
          /*side effect*/
          defaultMapValue.adjustedNodes =
            arcTailGet
            .get
            .adjustedNodes + arcHead
          /*return value*/
          defaultMapValue
        }*/
        /*side effect*/
        nodesWithAdjusted =
          //addArcHead
          nodesWithAdjusted
          .updated(
              key = arcTail,
              value = //valUpdated
                NodeMapValFieldsDynamic(
                                         //isExplored = false,
                                         nodeIndex =
                                           -1,
                                         //None,
                                         nodeLowLink = Int.MaxValue,
                                         //isInStack = false,
                                         //doneOrder = None,
                                         //groupLeader = None,
                                         adjustedNodes =
                                           arcTailGet
                                           .get
                                           .adjustedNodes :+ arcHead
                                       )
                  )
        /*return value*/
        //nodesWithAdjusted
      }
      /*return value*/
      //resultMapUpdated
    }
  }

  /*
  TODO may be use 'Buffer' or 'ArrayBuffer'
  as `appending` an item to an `array buffer` takes amortized `constant` time.
   */
  class DiGraphArray extends Graph {
    val defaultMapValue: NodeFieldsArray =
      NodeFieldsArray(
                       nodeKey =
                         -1,
                       nodeIndex =
                         -1,
                       nodeLowLink = Int.MaxValue,
                       adjustedNodes =
                         Stream
                         .empty
                     )
    /*current 'Array.length - 1' equal to max available key value*/
    var nodesSize: Int = 0
    /*override*/ var nodes:
    //scala.collection.mutable.ArrayBuffer[NodeFieldsArray] =
    Array[NodeFieldsArray] =
    /*first element not used
    to achieve 1-to-1 correspondence between
    array index & node key value
    */
    //Array(defaultMapValue)
    //ArrayBuffer(defaultMapValue)
    //ArrayBuffer.empty
      Array.empty

    //`Auxiliary Constructor`
    def this(minKeyVal: Int, maxKeyVal: Int) {
      //assume(maxKeyVal >= minKeyVal, s"at least one element")
      /*invoke the `primary constructor`*/
      this()
      /*'+1' for 'ArrayBuffer(0)'*/
      this.nodesSize = maxKeyVal - minKeyVal + 1 + 1
      this.nodes = nodes.padTo(nodesSize, defaultMapValue)
    }

    /*may be 'Unit' and not have return value*/
    def addEdge(
                 arcTail: Int,
                 arcHead: Int
                 //): Map[Int, NodeMapValFieldsDynamic] = {
                 ): Unit = {
      /*also:
      def isDefinedAt(idx: Int): Boolean
      Tests (bounds) whether this arraybuffer contains given index.
      def lengthCompare(len: Int): Int
      Compares the length of this arraybuffer to a test value.
      its running time is O(length min len) instead of O(length)
      * */
      //assume(nodes.isDefinedAt(arcTail),s"'Array' predefined, so must exist")
      val arcTailGet: NodeFieldsArray =
        nodes(arcTail)
      /*Option[NodeFieldsArray] =
        if (nodesSize >= arcTail) {
          /*must be in / within Array*/
          Some(nodes(arcTail))
        } else {
          /*undefined*/
          None
        }*/
      val arcHeadGet: NodeFieldsArray =
        nodes(arcHead)
      /*val arcHeadGet: Option[NodeFieldsArray] =
        if (nodesSize >= arcHead) {
          Some(nodes(arcHead))
        } else {
          /*undefined*/
          None
        }*/
      //val addArcHead: Map[Int, NodeMapValFieldsDynamic] =
      if (
        arcHeadGet.nodeKey == -1
      //arcHeadGet.isEmpty
      ) {
        /*add new 'node' / first occurrence*/
        /*side effect*/
        /*
        def append(elems: A*): Unit
        Appends the given elements to this buffer.
        def update(idx: Int, elem: A): Unit
        Replaces `element` at given `index` with a new `value`.
         */
        /*?changes all nodes value?*/
        //nodes(arcHead).nodeKey = arcHead
        nodes
        .update(
            /*idx =*/ arcHead,
            //elem =
            //defaultMapValue
            NodeFieldsArray(
                             nodeKey =
                               arcHead,
                             nodeIndex =
                               -1,
                             nodeLowLink = Int.MaxValue,
                             adjustedNodes =
                               Stream.empty
                           )
               )
        /*return value*/
        //nodesWithAdjusted
      } else /*if (arcTailGet.isDefined)*/ {
        /*same value*/
        //nodesWithAdjusted
      }
      //val resultMapUpdated: Map[Int, NodeMapValFieldsDynamic] =
      if (
        arcTailGet.nodeKey == -1
      //arcTailGet.isEmpty
      ) {
        /*add new 'node' / first occurrence*/
        /*val valToAdd: NodeMapValFieldsDynamic = {
          /*side effect*/
          defaultMapValue.adjustedNodes = Set(arcHead)
          /*return value*/
          defaultMapValue
        }*/
        /*side effect*/
        nodes
        .update(
            /*idx =*/ arcTail,
            /*elem =*/
            NodeFieldsArray(
                             nodeKey =
                               arcTail,
                             nodeIndex =
                               -1,
                             nodeLowLink = Int.MaxValue,
                             adjustedNodes =
                               Stream(arcHead)
                           )
               )
        /*return value*/
        //nodesWithAdjusted
      } else /*if (arcTailGet.isDefined)*/ {
        /*add new adjusted 'node' to existed 'node'*/
        /*val valUpdated: NodeMapValFieldsDynamic = {
          /*side effect*/
          defaultMapValue.adjustedNodes =
            arcTailGet
            .get
            .adjustedNodes + arcHead
          /*return value*/
          defaultMapValue
        }*/
        /*side effect*/
        nodes(arcTail)
        .adjustedNodes =
          arcTailGet
          //.get
          .adjustedNodes :+ arcHead

        /*return value*/
        //nodesWithAdjusted
      }
      /*return value*/
      //resultMapUpdated
    }
  }

  /*the companion object of the class*/
  object DiGraphArray {
    //factory methods, extractors, apply
    /*`parameters` only for 'case classes'*/
    //def apply() = new DiGraphArray("<no name>", 0)
    //def apply(name: String) = new DiGraphArray(name, 0)
    //Extractor: Create tokens that represent your object
    /*def unapply(x: Employee) =
      Some(x.lastName, x.middleName, x.firstName)*/
    /*Factory*/
    def init(minKeyVal: Int, maxKeyVal: Int): DiGraphArray = {
      //assume(maxKeyVal >= minKeyVal, s"at least one element")
      /*invoke the `primary constructor`*/
      val resultObject: DiGraphArray = new DiGraphArray
      /*'+1' for 'ArrayBuffer(0)'*/
      val nodesSize = maxKeyVal - minKeyVal + 1 + 1
      val bufferArray: collection.mutable.ArrayBuffer[NodeFieldsArray] =
        new ArrayBuffer[NodeFieldsArray]
        //.empty
        .padTo(nodesSize, resultObject.defaultMapValue)
      /*side effect*/
      resultObject.nodesSize = nodesSize
      resultObject.nodes = bufferArray.toArray
      /*return value*/
      resultObject
    }
  }

  /*?may be this
  'seq.view.map(f).flatMap(g).filter(p).toList'
  can speedUp computation ?
  * */
  def collectInducedArcs(
                          arcs: Vector[ArcFromNodes],
                          tailValue: Int
                          ): Vector[ArcFromNodes] =
    arcs
    .collect(
    { case a if (
      /*start with explored 'node'*/
      a.arcTail.node == tailValue &&
        /*where 'arc's 'head' yet unexplored*/
        !a.arcHead.isExplored
      ) => a
    }
            )

  /*instead of 'filter'*/
  /*return: Option or just empty / nonEmpty*/
  def pickAllInducedArcs(
                          arcs: Vector[ArcFromNodes],
                          tailValue: Int
                          ): Vector[ArcFromNodes] = {
    //): Option[Vector[ArcFromNodes]] = {
    def innerLoop(
                   accum: Vector[ArcFromNodes] =
                   Vector.empty,
                   currentIndex: Int,
                   arcLength: Int = 0,
                   direction: TraverseDirection
                   ): Vector[ArcFromNodes] = {
      if (accum.isEmpty) {
        /*first step / iteration*/
        /*recursion*/
        innerLoop(
                   /*'prepend' ?order irrelevant?*/
                   arcs(currentIndex) +: accum,
                   currentIndex =
                     if (direction == Up) {
                       /*may be out of bound*/
                       currentIndex - 1
                     } else /*if (direction == Down)*/ {
                       currentIndex + 1
                     },
                   arcLength = arcLength,
                   direction: TraverseDirection
                 )
      } else /*if (accum.isEmpty)*/ {
        if (
          currentIndex < 0 ||
            currentIndex >= arcLength
        ) {
          /*out of bound*/
          /*return value*/
          accum
        } else {
          val nextArc: ArcFromNodes =
            arcs(currentIndex)

          if (nextArc.arcTail.node != tailValue) {
            /*sequence stop*/
            accum
          } else {
            /*recursion*/
            innerLoop(
                       nextArc +: accum,
                       currentIndex =
                         if (direction == Up) {
                           currentIndex - 1
                         } else /*if (direction == Down)*/ {
                           currentIndex + 1
                         },
                       arcLength = arcLength,
                       direction = direction
                     )
          }
        }
      }
    }

    /*find first or any occurrence*/
    val searchResult: Option[Int] =
      randomSearchForArcTailIndex(
                                   arcs: Vector[ArcFromNodes],
                                   tailValue: Int
                                 )

    if (searchResult.isEmpty) {
      /*return value*/
      //None
      //accum
      Vector.empty[ArcFromNodes]
    } else {
      /*inner loop*/
      /*for first occurrence*/
      /*initialize*/
      /*contain at least one element*/
      //Some(
      innerLoop(
                 currentIndex =
                   searchResult.get,
                 arcLength = arcs.length,
                 direction = Down
               )
      //)
    }
  }

  /*extract 'nodes' & 'arcs' from 'FileContent' strings*/
  /*? assume, all arcs are distinct & unique ?*/
  /*test reveals existence of cyclic self pointed arcs*/
  @scala.annotation.tailrec
  def extractArcs(
                   fileContentIter: Iterator[String],
                   arcs: Vector[Arc] =
                   Vector.empty[Arc] //,
                   /*'nodes' is just a range from '1' to '875714'*/
                   /*nodes: Vector[Int] =
                   Vector.empty[Int]*/
                   ): Vector[Arc] = {
    if (fileContentIter.isEmpty) {
      /*return value*/
      arcs
    } else /*if (adjacencyList.hasNext)*/ {
      //val stringSplit: Array[Int] =
      val Array(tail, head) =
        fileContentIter
        .next()
        //CHARACTER TABULATION
        //.split('\u0009')
        .split(" ")
        .map(_.toInt)
      /*val currentTailNode: Int =
        stringSplit
        .head
      val currentHeadNode: Int =
        stringSplit
        .tail
        .head*/
      /*inner loop*/
      //val newArcs: Vector[Arc] =
      /*recursion*/
      extractArcs(
                   /*reduced already by '.next()'*/
                   fileContentIter,
                   /*? order does not matter because of random node merge ?*/
                   //Arc(currentTailNode, currentHeadNode) +: arcs
                   Arc(tail, head) +: arcs
                 )
    }
  }

  /*extract sorted / preserve original order 'arcs' from 'FileContent' strings*/
  /*? assume, all arcs are distinct & unique ?*/
  /*test reveals existence of cyclic self pointed arcs*/
  @scala.annotation.tailrec
  def extractSortedArcs(
                         fileContentIter: Iterator[String],
                         arcs: Vector[Arc] =
                         /*Performance on: head, tail, apply, update,
                         prepend, append
                         * 'eC'	- The operation takes `effectively constant`
                         * time*/
                         Vector.empty[Arc] //,
                         /*'nodes' is just a range from '1' to '875714'*/
                         ): Vector[Arc] = {
    if (fileContentIter.isEmpty) {
      /*return value*/
      arcs
    } else /*if (adjacencyList.hasNext)*/ {
      val Array(tail, head) =
        fileContentIter
        .next()
        .split(" ")
        .map(_.toInt)
      /*recursion*/
      extractSortedArcs(
                         /*reduced already by '.next()'*/
                         fileContentIter,
                         /*'Stack' on: 'append' 'C'	The operation takes `
                         (fast) constant` time*/
                         /*keep it sorted, like in the input*/
                         arcs :+ Arc(tail, head)
                       )
    }
  }

  /*extract 'nodes' & 'arcs' from 'FileContent' strings*/
  /*? assume, all arcs are distinct & unique ?*/
  /*test reveals existence of cyclic self pointed arcs*/
  @scala.annotation.tailrec
  def extractArcsAndNodes(
                           fileContentIter: Iterator[String],
                           /*'nodes' is just a range from '1' to '875714'*/
                           /*or
                           keep tracking max node value
                           assuming that nodes are consecutive
                           5819251(number of edges doubled?) did not equal
                           875714
                           * */
                           /*nodes: Vector[Int] =
                           Vector.empty[Int]*/
                           /*nodes: Seq[Int] =
                         Seq.empty[Int],*/
                           arcs: Vector[Arc] =
                           Vector.empty[Arc],
                           //lastTail: Option[Int] = None,
                           minNode: Int = Double.PositiveInfinity.toInt,
                           maxNode: Int = Double.NegativeInfinity.toInt
                           //): (Vector[Int], Vector[Arc]) = {
                           ): DirectedGraph = {
    if (fileContentIter.isEmpty) {
      /*return value*/
      //(nodes, arcs)
      //DirectedGraph(nodes, arcs)
      DirectedGraph(minNode to maxNode, arcs)
      /*DirectedGraph(
                     nodes.distinct,
                     arcs)*/
    } else /*if (adjacencyList.hasNext)*/ {
      val Array(tail, head) =
        fileContentIter
        .next()
        //CHARACTER TABULATION
        //.split('\u0009')
        .split(" ")
        .map(_.toInt)
      val newArcs: Vector[Arc] =
      /*skip self loop*/
        if (tail == head) {
          arcs
        } else {
          /*for 'Vector' 'prepend', 'append'
                   operation takes ?same? `effectively constant` time*/
          //'prepend'
          //Arc(tail, head) +: arcs
          //'append'
          arcs :+ Arc(tail, head)
        }
      /*very expensive & highly time consuming computation*/
      /*val hasTail: Boolean =
        nodes.contains(tail)
      val hasHead: Boolean =
        nodes.contains(head)*/
      /*
      A `memory leak` happens when
      the application creates
      more and more objects and
      never releases them.
      The garbage collector
      cannot collect those objects and
      the application will eventually run out of memory.
      At this point,
      the JVM will throw an OOM (OutOfMemoryError).
       */
      //val newNodes: Seq[Int] =
      /*skip self loop*/
      /*if (hasTail && hasHead) {
        nodes
      } else if (hasTail && !hasHead) {
        head +: nodes
      } else if (!hasTail && hasHead) {
        tail +: nodes
      } else /*if (!hasTail && !hasHead)*/ {
        tail +: head +: nodes
      }*/
      /*assume that input sorted by 'tails'
      * so they are consecutive / in ascending order*/
      //val addTail: Vector[Int] =
      /*if (
      //lastTail.isEmpty ||
      //Some(tail) == lastTail
        lastTail.contains(tail)
      ) {
        //nodes
        if (
          tail == head ||
            lastTail.contains(head)
        ) {
          nodes
        } else {
          head +: nodes
        }
      } else {
        /*how mach time consume new collection creation ?*/
        //tail +: nodes
        if (
          tail == head ||
            lastTail.contains(head)
        ) {
          tail +: nodes
        } else {
          head +: tail +: nodes
        }
      }*/
      /*val addHead: Vector[Int] =
        if (
          tail == head ||
            lastTail.contains(head)
        ) {
          addTail
        } else {
          head +: addTail
        }*/
      val newMin: Int =
        if (minNode > tail && tail <= head) {
          tail
        } else if (minNode > tail && tail > head) {
          head
        } else /*if(minNode < tail || minNodel < head)*/ {
          minNode
        }
      val newMax: Int =
        if (maxNode < tail && tail >= head) {
          tail
        } else if (maxNode < tail && tail < head) {
          head
        } else /*if(maxNode > tail || maxNode > head)*/ {
          maxNode
        }
      /*recursion*/
      extractArcsAndNodes(
                           /*reduced already by '.next()'*/
                           fileContentIter,
                           //newNodes,
                           //addHead,
                           newArcs,
                           //Some(tail)
                           newMin,
                           newMax
                         )
    }
  }

  /*extract 'nodes' from 'arcs'*/
  //@scala.annotation.tailrec
  def extractNodesFromArcs(
                            /*nodes: Vector[Int] =
                            Vector.empty[Int],*/
                            arcs: Vector[Arc]
                            ): Vector[Int] = {
    val tails: Vector[Int] =
      for {arc <- arcs} yield arc.arcTail
    val heads: Vector[Int] =
      for {arc <- arcs} yield arc.arcHead
    val nodes: Vector[Int] =
      tails.union(heads).distinct
    /*return value*/
    nodes
  }

  /*order of 'arcs' does not matter*/
  /*assume that 'nodes.value' unique*/
  /*assume that 'Iterator' contains only pair Int in String*/
  @scala.annotation.tailrec
  def fillDirectedGraphDynamicFromArcs(
                                        fileContentIter: Iterator[String],
                                        result: DirectedGraphDynamic =
                                        new DirectedGraphDynamic,
                                        pattern: Regex =
                                        """\d+""".r,
                                        //isArcsReversed: Boolean = false
                                        nonReversedArcs: Boolean = true
                                        ): DirectedGraphDynamic = {
    if (fileContentIter.isEmpty) {
      /*return value*/
      result
    } else /*if (adjacencyList.hasNext)*/ {
      /*may be leading 'space' then
      * delimiter one or double 'space'*/
      val List(first, second): List[Int] =
        pattern
        .findAllIn(fileContentIter
                   .next())
        .map(_.toInt)
        .toList
      val List(arcTail, arcHead): List[Int] =
        if (nonReversedArcs) {
          List(first, second)
        } else {
          /*return value*/
          List(second, first)
        }
      /*side effect*/
      result
      .addEdge(
          arcTail = arcTail,
          arcHead = arcHead
              )
      /*recursion*/
      fillDirectedGraphDynamicFromArcs(
                                        fileContentIter: Iterator[String],
                                        result = result,
                                        nonReversedArcs = nonReversedArcs
                                      )
    }
  }

  /*order of 'arcs' does not matter*/
  /*assume that 'nodes.value' unique*/
  /*assume that 'Iterator' contains only pair Int in String*/
  @scala.annotation.tailrec
  def fillDiGraphArrayWithArcs(
                                fileContentIter: Iterator[String],
                                result: DiGraphArray =
                                new DiGraphArray,
                                pattern: Regex =
                                """\d+""".r,
                                //isArcsReversed: Boolean = false
                                nonReversedArcs: Boolean = true
                                ): DiGraphArray = {
    if (fileContentIter.isEmpty) {
      /*return value*/
      result
    } else /*if (adjacencyList.hasNext)*/ {
      /*may be leading 'space' then
      * delimiter one or double 'space'*/
      val List(first, second): List[Int] =
        pattern
        .findAllIn(fileContentIter
                   .next())
        .map(_.toInt)
        .toList
      val List(arcTail, arcHead): List[Int] =
        if (nonReversedArcs) {
          List(first, second)
        } else {
          /*return value*/
          List(second, first)
        }
      /*side effect*/
      result
      .addEdge(
          arcTail = arcTail,
          arcHead = arcHead
              )
      /*recursion*/
      fillDiGraphArrayWithArcs(
                                fileContentIter: Iterator[String],
                                result = result,
                                nonReversedArcs = nonReversedArcs
                              )
    }
  }

  /*order of 'arcs' does not matter*/
  /*assume that 'nodes.value' unique*/
  /*assume that 'Iterator' contains only pair Int in String*/
  @scala.annotation.tailrec
  def fillSetsMapFromArcs(
                           fileContentIter: Iterator[String],
                           resultMap:
                           DiGraphSetsMap =
                           new DiGraphSetsMap,
                           /*Map[Int, Set[Int]] =
                           Map.empty,*/
                           pattern: Regex =
                           """\d+""".r,
                           //isArcsReversed: Boolean = false
                           nonReversedArcs: Boolean = true
                           ): DiGraphSetsMap = {
                           //): Map[Int, Set[Int]] = {
    if (fileContentIter.isEmpty) {
      /*return value*/
      resultMap
    } else /*if (adjacencyList.hasNext)*/ {
      /*may be leading 'space' then
      * delimiter one or double 'space'*/
      val List(first, second): List[Int] =
        pattern
        .findAllIn(fileContentIter
                   .next())
        .map(_.toInt)
        .toList
      val List(arcTail, arcHead): List[Int] =
        if (nonReversedArcs) {
          List(first, second)
        } else {
          /*return value*/
          List(second, first)
        }
      /*side effect*/
      resultMap
      .addEdge(
          arcTail = arcTail,
          arcHead = arcHead
              )
      /*recursion*/
      fillSetsMapFromArcs(
                           fileContentIter: Iterator[String],
                           resultMap =
                             resultMap,
                           nonReversedArcs = nonReversedArcs
                         )
    }
  }

  /*order of 'arcs' does not matter*/
  /*assume that 'nodes.value' unique*/
  /*assume that 'Iterator' contains only pair Int in String*/
  @scala.annotation.tailrec
  def makeAdjacencyListMapFromArcs(
                                    fileContentIter: Iterator[String],
                                    resultMap: Map[Int,
                                      NodeMapValFieldsStatic] =
                                    Map.empty,
                                    pattern: Regex =
                                    """\d+""".r,
                                    //isArcsReversed: Boolean = false
                                    nonReversedArcs: Boolean = true
                                    ): Map[Int, NodeMapValFieldsStatic] = {
    /*
      cases:
      `arcTail` new => add to Map
      `arcHead` new =>
      add to Map
      add to existing key.adjustedNodes
       */
    if (fileContentIter.isEmpty) {
      /*return value*/
      resultMap
    } else /*if (adjacencyList.hasNext)*/ {
      /*may be leading 'space' then
      * delimiter one or double 'space'*/
      val List(first, second): List[Int] =
        pattern
        .findAllIn(fileContentIter
                   .next())
        .map(_.toInt)
        .toList
      val List(arcTail, arcHead): List[Int] =
        if (nonReversedArcs) {
          List(first, second)
        } else {
          /*return value*/
          List(second, first)
        }
      val arcTailGet: Option[NodeMapValFieldsStatic] =
        resultMap
        .get(key = arcTail)
      val arcHeadlGet: Option[NodeMapValFieldsStatic] =
        resultMap
        .get(key = arcHead)
      val addArcHead: Map[Int, NodeMapValFieldsStatic] =
        if (arcHeadlGet.isEmpty) {
          /*add new 'node' / first occurrence*/
          resultMap
          .updated(
              key = arcHead,
              value =
                NodeMapValFieldsStatic(
                                        isExplored = false,
                                        doneOrder = None,
                                        adjustedNodes = Set.empty))
        } else /*if (arcTailGet.isDefined)*/ {
          /*same value*/
          resultMap
        }
      val resultMapUpdated: Map[Int, NodeMapValFieldsStatic] =
        if (arcTailGet.isEmpty) {
          /*add new 'node' / first occurrence*/
          addArcHead
          .updated(
              key = arcTail,
              value =
                NodeMapValFieldsStatic(
                                        isExplored = false,
                                        doneOrder = None,
                                        adjustedNodes = Set(arcHead)))
        } else /*if (arcTailGet.isDefined)*/ {
          /*add new adjusted 'node' to existed 'node'*/
          addArcHead
          .updated(
              key = arcTail,
              value =
                NodeMapValFieldsStatic(
                                        isExplored = false,
                                        doneOrder = None,
                                        adjustedNodes =
                                          arcTailGet
                                          .get
                                          .adjustedNodes + arcHead
                                      )
                  )
        }
      /*recursion*/
      makeAdjacencyListMapFromArcs(
                                    fileContentIter: Iterator[String],
                                    resultMap =
                                      resultMapUpdated,
                                    nonReversedArcs = nonReversedArcs
                                  )
    }
  }

  /*assume that 'arcsRemains.sorted' by 'tail'*/
  /*'sink' nodes must be included with empty 'AdjacencyList',
  * so
   * must iterate over all 'nodes', not 'arcs'
   * presume that 'nodes' form range / consequential interval
  * */
  @scala.annotation.tailrec
  def makeAdjacencyListFromArcs(
                                 //nodesRemains: List[Int],
                                 minNodeVal: Int,
                                 maxNodeVal: Int,
                                 /*at start equal 'minNodeVal' - '1'*/
                                 /*counter of added 'nodes'*/
                                 /*must exceed 'maxNodeVal' eventually*/
                                 currentNodeVal: Int,
                                 /*currentNodeVal: Option[Int] =
                                 None,*/
                                 /*'List' has faster 'head', 'tail' than
                                 'Vector'*/
                                 /*must be empty eventually*/
                                 arcsRemains: List[Arc],
                                 /*result is 'Vector' for `fast` 'apply'*/
                                 adjustedNodes: Vector[AdjacencyListElem] =
                                 Vector.empty,
                                 /*?pointer?accum to add later to
                                 adjustedNodes*/
                                 nodeToAdd: Option[AdjacencyListElem] =
                                 None
                                 ): Vector[AdjacencyListElem] = {
    /*
      cases:
      >1>currentNodeVal == maxNodeVal (&& arcsRemains.isEmpty)
      => Done,
      =>add 'nodeToAdd' if any to 'adjustedNodes'
      return 'adjustedNodes'
      >2>currentNodeVal < maxNodeVal &&
      (arcsRemains.nonEmpty &&)
      same sequence, same 'arcTail'
      'arcsRemains.head.arcTail' == 'currentNodeVal'
      =>updated element => add new adjusted 'node' to list
      'nodeToAdd' =
      AdjacencyListElem(
      'nodeToAdd.node',
      'nodeToAdd.adjustedNodes' :+ 'arcsRemains.head.arcHead'
      )'
      'newCurrentNodeVal' = 'currentNodeVal'
      'arcsRemains' = 'arcsRemains.tail'
      >3>currentNodeVal < maxNodeVal &&
      (arcsRemains.nonEmpty &&)
      'arcsRemains.head.arcTail' == 'currentNodeVal' + 1
      =>add 'nodeToAdd' if any to 'adjustedNodes'
      'nodeToAdd' =
      AdjacencyListElem(
      IsExploredNode('arcsRemains.head.arcTail',false),
      List('arcsRemains.head.arcHead')
      )'
      'newCurrentNodeVal' = 'currentNodeVal' + '1'
      'arcsRemains' = 'arcsRemains.tail'
      >4>currentNodeVal < maxNodeVal &&
      (arcsRemains.nonEmpty &&)
      'arcsRemains.head.arcTail' > 'currentNodeVal' + '1'
      =>add 'nodeToAdd' if any to 'adjustedNodes'
      'nodeToAdd' =
      AdjacencyListElem(
      IsExploredNode('currentNodeVal' + '1',false),
      List.empty
      )'
      'newCurrentNodeVal' = 'currentNodeVal' + '1'
      'arcsRemains' = 'arcsRemains'
      >5>currentNodeVal < maxNodeVal &&
      arcsRemains.isEmpty &&
      =>add 'nodeToAdd' if any to 'adjustedNodes'
      'nodeToAdd' =
      AdjacencyListElem(
      IsExploredNode('currentNodeVal' + '1',false),
      List.empty
      )'
      'newCurrentNodeVal' = 'currentNodeVal' + '1'
      'arcsRemains' = 'arcsRemains'
       */
    if (
      currentNodeVal >= maxNodeVal /*||
      arcsRemains.isEmpty*/
    ) {
      /*return value*/
      if (nodeToAdd.isEmpty) {
        adjustedNodes
      } else {
        adjustedNodes :+ nodeToAdd.get
      }
    } else if (
             currentNodeVal < maxNodeVal &&
               arcsRemains.nonEmpty &&
               arcsRemains.head.arcTail == currentNodeVal &&
               /*must be*/
               nodeToAdd.isDefined
           ) {
      //same sequence, same 'arcTail'
      //=>update element => add new adjusted 'node' to list
      val nodeToAddValue =
        nodeToAdd.get
      val newNodeToAdd =
        AdjacencyListElem(
                           nodeToAddValue.node,
                           nodeToAddValue.adjustedNodes :+
                             arcsRemains.head.arcHead
                         )
      val newCurrentNodeVal = currentNodeVal
      val newArcsRemains = arcsRemains.tail
      val newAdjustedNodes = adjustedNodes

      /*recursion*/
      makeAdjacencyListFromArcs(
                                 minNodeVal = minNodeVal,
                                 maxNodeVal = maxNodeVal,
                                 currentNodeVal = newCurrentNodeVal,
                                 arcsRemains =
                                   newArcsRemains,
                                 adjustedNodes =
                                   newAdjustedNodes,
                                 nodeToAdd =
                                   Some(newNodeToAdd)
                               )
    } else if (
             currentNodeVal < maxNodeVal &&
               arcsRemains.nonEmpty &&
               arcsRemains.head.arcTail == currentNodeVal + 1 /*&&
               /*may be*/
               nodeToAdd.isDefined*/
           ) {
      //new 'arcTail'
      //=>add 'nodeToAdd' if any to 'adjustedNodes'
      val newAdjustedNodes =
        if (nodeToAdd.isEmpty) {
          adjustedNodes
        } else {
          adjustedNodes :+ nodeToAdd.get
        }
      /*val nodeToAddValue =
        nodeToAdd.get*/
      val newNodeToAdd =
        AdjacencyListElem(
                           IsExploredNode(arcsRemains.head.arcTail,
                                          isExplored = false),
                           List(arcsRemains.head.arcHead)
                         )
      val newCurrentNodeVal = currentNodeVal + 1
      val newArcsRemains = arcsRemains.tail

      /*recursion*/
      makeAdjacencyListFromArcs(
                                 minNodeVal = minNodeVal,
                                 maxNodeVal = maxNodeVal,
                                 currentNodeVal = newCurrentNodeVal,
                                 arcsRemains =
                                   newArcsRemains,
                                 adjustedNodes =
                                   newAdjustedNodes,
                                 nodeToAdd =
                                   Some(newNodeToAdd)
                               )
    } else /*if (
             currentNodeVal < maxNodeVal &&
               (arcsRemains.isEmpty ||
               (arcsRemains.head.arcTail >= currentNodeVal + 1 ))
           )*/ {
      //next 'nodeVal'
      //=>add 'nodeToAdd' if any to 'adjustedNodes'
      val newAdjustedNodes =
        if (nodeToAdd.isEmpty) {
          adjustedNodes
        } else {
          adjustedNodes :+ nodeToAdd.get
        }
      val newNodeToAdd =
        AdjacencyListElem(
                           IsExploredNode(currentNodeVal + 1,
                                          isExplored = false),
                           List()
                         )
      val newCurrentNodeVal = currentNodeVal + 1
      val newArcsRemains = arcsRemains

      /*recursion*/
      makeAdjacencyListFromArcs(
                                 minNodeVal = minNodeVal,
                                 maxNodeVal = maxNodeVal,
                                 currentNodeVal = newCurrentNodeVal,
                                 arcsRemains =
                                   newArcsRemains,
                                 adjustedNodes =
                                   newAdjustedNodes,
                                 nodeToAdd =
                                   Some(newNodeToAdd)
                               )
    }
  }

  /*assume that 'arcsRemains.sorted' by 'tail'*/
  /*assume that 'nodes' form range / consequential interval*/
  /*prerequisite: nodes[IndexedNode] sorted by 'nodeVal'*/
  @scala.annotation.tailrec
  def makeIndexedNodeWithAdjacencyListFromArcs(
                                                /*for look up*/
                                                nodes: Vector[IndexedNode] =
                                                Vector.empty,
                                                /*for use 'head','tail'*/
                                                nodesRemains:
                                                List[IndexedNode] =
                                                List.empty,
                                                /*at start equal 'minNodeVal'
                                                 - '1'*/
                                                /*counter of added 'nodes'*/
                                                /*must exceed 'maxNodeVal'
                                                eventually*/
                                                currentNodeVal: Int,
                                                /*must be empty eventually*/
                                                arcsRemains: List[Arc],
                                                /*result is 'Vector' for
                                                          *  */
                                                adjustedNodes:
                                                Vector[IndexedNodeWithAdjacencyList] =
                                                Vector.empty,
                                                /*accum to add later to
                                                'adjustedNodes'*/
                                                nodeToAdd:
                                                Option[IndexedNodeWithAdjacencyList] =
                                                None
                                                ):
  Vector[IndexedNodeWithAdjacencyList] = {
    if (
      nodesRemains.isEmpty
    //currentNodeVal >= maxNodeVal /*||
    //arcsRemains.isEmpty*/
    ) {
      /*return value*/
      if (nodeToAdd.isEmpty) {
        adjustedNodes
      } else {
        adjustedNodes :+ nodeToAdd.get
      }
    } else if (
           //currentNodeVal < maxNodeVal &&
             arcsRemains.nonEmpty &&
               arcsRemains.head.arcTail == currentNodeVal &&
               /*must be*/
               nodeToAdd.isDefined
           ) {
      //same sequence, same 'arcTail'
      //=>update element => add new adjusted 'node' to list
      val nodeToAddValue =
        nodeToAdd.get
      val newNodeToAdd =
        IndexedNodeWithAdjacencyList(
                                      nodeToAddValue.node,
                                      nodeToAddValue.adjustedNodes :+
                                        nodes(arcsRemains.head.arcHead - 1)
                                    )
      val newCurrentNodeVal = currentNodeVal
      val newArcsRemains = arcsRemains.tail
      val newAdjustedNodes = adjustedNodes

      /*recursion*/
      makeIndexedNodeWithAdjacencyListFromArcs(
                                                nodes = nodes,
                                                nodesRemains = nodesRemains,
                                                currentNodeVal =
                                                  newCurrentNodeVal,
                                                arcsRemains =
                                                  newArcsRemains,
                                                adjustedNodes =
                                                  newAdjustedNodes,
                                                nodeToAdd =
                                                  Some(newNodeToAdd)
                                              )
    } else if (
           //currentNodeVal < maxNodeVal &&
             arcsRemains.nonEmpty &&
               arcsRemains.head.arcTail == currentNodeVal + 1 /*&&
               /*may be*/
               nodeToAdd.isDefined*/
           ) {
      //new 'arcTail'
      //=>add 'nodeToAdd' if any to 'adjustedNodes'
      val newAdjustedNodes =
        if (nodeToAdd.isEmpty) {
          adjustedNodes
        } else {
          adjustedNodes :+ nodeToAdd.get
        }
      /*val nodeToAddValue =
        nodeToAdd.get*/
      val newNodeToAdd =
        IndexedNodeWithAdjacencyList(
                                      nodes(arcsRemains.head.arcTail - 1),
                                      List(nodes(arcsRemains.head.arcHead - 1))
                                    )
      val newCurrentNodeVal =
        currentNodeVal + 1
      //nodesRemains.head.nodeVal
      val newArcsRemains = arcsRemains.tail

      /*recursion*/
      makeIndexedNodeWithAdjacencyListFromArcs(
                                                nodes = nodes,
                                                nodesRemains = nodesRemains
                                                               .tail,
                                                currentNodeVal =
                                                  newCurrentNodeVal,
                                                arcsRemains =
                                                  newArcsRemains,
                                                adjustedNodes =
                                                  newAdjustedNodes,
                                                nodeToAdd =
                                                  Some(newNodeToAdd)
                                              )
    } else /*if (
             currentNodeVal < maxNodeVal &&
               (arcsRemains.isEmpty ||
               (arcsRemains.head.arcTail >= currentNodeVal + 1 ))
           )*/ {
      //next 'nodeVal'
      //=>add 'nodeToAdd' if any to 'adjustedNodes'
      val newAdjustedNodes =
        if (nodeToAdd.isEmpty) {
          adjustedNodes
        } else {
          adjustedNodes :+ nodeToAdd.get
        }
      val newNodeToAdd =
        IndexedNodeWithAdjacencyList(
                                      nodes(currentNodeVal),
                                      List()
                                    )
      val newCurrentNodeVal = currentNodeVal + 1
      val newArcsRemains = arcsRemains

      /*recursion*/
      makeIndexedNodeWithAdjacencyListFromArcs(
                                                nodes = nodes,
                                                nodesRemains = nodesRemains
                                                               .tail,
                                                currentNodeVal =
                                                  newCurrentNodeVal,
                                                arcsRemains =
                                                  newArcsRemains,
                                                adjustedNodes =
                                                  newAdjustedNodes,
                                                nodeToAdd =
                                                  Some(newNodeToAdd)
                                              )
    }
  }

  /*assume that 'arcsRemains.sorted' by 'tail'*/
  /*assume that 'nodes' form range / consequential interval*/
  /*prerequisite: nodes[IndexedNode] sorted by 'nodeVal'*/
  @scala.annotation.tailrec
  def makeExplorableAdjacencyListFromArcs(
                                           /*for look up
                                           * may be created dynamically
                                           * from scratch*/
                                           nodes: => Vector[IsExploredNode] =
                                           Vector.empty,
                                           /*for use 'head','tail'*/
                                           /*may be redundant*/
                                           //nodesRemains: =>
                                           /*List[IsExploredNode] =
                                           List.empty,*/
                                           /*Stream[IsExploredNode] =
                                           Stream.empty,*/
                                           /*at start equal
                                           'minNodeVal' - '1'
                                           or some outOfBound value
                                           */
                                           /*counter of added 'nodes'*/
                                           /*must exceed 'maxNodeVal'
                                           eventually*/
                                           /*may be redundant*/
                                           currentNodeVal: Int,
                                           /*must be empty eventually*/
                                           //arcsRemains: List[Arc],
                                           /*must be 'val' not 'def' as
                                           it used in comparisons */
                                           arcsRemains: => Stream[Arc],
                                           /*resulting 'Vector'*/
                                           adjustedNodes:
                                           Vector[ExplorableNodeWithAdjusted] =
                                           Vector.empty,
                                           /*accum to add later to
                                           'adjustedNodes'
                                           also may be used to
                                           check vs. current value
                                           is it new or same*/
                                           nodeToAdd:
                                           Option[ExplorableNodeWithAdjusted] =
                                           None,
                                           /*used to start 'nodes' range
                                           * also to define shift vs. nodes
                                           * index
                                           * if > 0 then
                                           * index =(currentNodeVal -
                                           * minNodeVal)
                                           * if == 0 then
                                           * index = currentNodeVal
                                           * if < 0 then
                                           * index =(currentNodeVal +
                                           * minNodeVal)
                                           * */
                                           minNodeVal: Int = 1,
                                           /*used to stop execution
                                           *and add missing `sink` 'nodes'
                                           * with empty adjusted list
                                           * maxNodeVal = rangeSize - 1
                                           * or computed within method calls
                                           * provided that input 'arc's
                                           * sorted by 'arcTail'
                                           * */
                                           maxNodeVal: Int = Int.MaxValue,
                                           //nodesCount: Int = 0,
                                           /*rangeSize*/
                                           //nodesAmount: Int = 1,
                                           nodeIndexShift: Int = -1
                                           ):
  Vector[ExplorableNodeWithAdjusted] = {
    /*cases:
    * */
    if (
    //nodesRemains.isEmpty
    //nodesCount >= maxNodeVal
      currentNodeVal >= maxNodeVal
    ) {
      /*return value*/
      if (nodeToAdd.isEmpty) {
        adjustedNodes
      } else {
        adjustedNodes :+ nodeToAdd.get
      }
    } else if (
             arcsRemains.nonEmpty &&
               arcsRemains.head.arcTail == currentNodeVal &&
               //arcsRemains.head.arcTail == nodesRemains.head &&
               /*must be*/
               nodeToAdd.isDefined
           ) {
      //same sequence, same 'arcTail'
      //=>update element => add new adjusted 'node' to list
      val nodeToAddValue: ExplorableNodeWithAdjusted =
        nodeToAdd.get
      val newNodeToAdd: ExplorableNodeWithAdjusted =
        ExplorableNodeWithAdjusted(
                                    nodeToAddValue.node,
                                    nodeToAddValue.adjustedNodes :+
                                      nodes(
                                             arcsRemains
                                             .head
                                             .arcHead + nodeIndexShift)
                                  )
      val newCurrentNodeVal: Int =
        currentNodeVal
      //nodesRemains.head.node
      //def newArcsRemains = arcsRemains.tail
      //val nodesCountUpdated: Int = nodesCount + 1
      //val newAdjustedNodes = adjustedNodes

      /*recursion*/
      makeExplorableAdjacencyListFromArcs(
                                           nodes = nodes,
                                           //nodesRemains = nodesRemains,
                                           currentNodeVal =
                                             /*same*/
                                             currentNodeVal,
                                           arcsRemains =
                                             arcsRemains.tail,
                                           //newArcsRemains,
                                           adjustedNodes =
                                             adjustedNodes,
                                           //newAdjustedNodes,
                                           nodeToAdd =
                                             Some(newNodeToAdd),
                                           maxNodeVal = maxNodeVal,
                                           //nodesCount = nodesCountUpdated,
                                           nodeIndexShift = nodeIndexShift
                                         )
    } else if (
           //currentNodeVal < maxNodeVal &&
             arcsRemains.nonEmpty &&
               arcsRemains.head.arcTail == currentNodeVal + 1 /*&&
               //arcsRemains.head.arcTail == nodesRemains.head.node + 1
               /*may be*/
               //nodeToAdd.isDefined*/
           ) {
      //new 'arcTail'
      //=>add 'nodeToAdd' if any to 'adjustedNodes'
      def newAdjustedNodes: Vector[ExplorableNodeWithAdjusted] =
        if (nodeToAdd.isEmpty) {
          adjustedNodes
        } else /*if (nodeToAdd.isDefined)*/ {
          adjustedNodes :+ nodeToAdd.get
        }
      /*val nodeToAddValue =
        nodeToAdd.get*/
      def newNodeToAdd: ExplorableNodeWithAdjusted =
        ExplorableNodeWithAdjusted(
                                    nodes(
                                           arcsRemains
                                           .head
                                           .arcTail + nodeIndexShift),
                                    //List(nodes(arcsRemains.head.arcHead - 1))
                                    Stream(
                                            nodes(
                                                   arcsRemains
                                                   .head
                                                   .arcHead + nodeIndexShift))
                                  )
      val newCurrentNodeVal: Int =
      /*incremented*/
        currentNodeVal + 1
      //nodesRemains.head.node
      //nodesRemains.head.nodeVal
      //def newArcsRemains: Stream[Arc] = arcsRemains.tail
      //val nodesCountUpdated: Int = nodesCount + 1

      /*recursion*/
      makeExplorableAdjacencyListFromArcs(
                                           nodes = nodes,
                                           /*nodesRemains =
                                             nodesRemains
                                                          .tail,*/
                                           currentNodeVal =
                                             currentNodeVal + 1,
                                           //newCurrentNodeVal,
                                           arcsRemains =
                                             arcsRemains.tail,
                                           //newArcsRemains,
                                           adjustedNodes =
                                             newAdjustedNodes,
                                           nodeToAdd =
                                             Some(newNodeToAdd),
                                           maxNodeVal = maxNodeVal,
                                           //nodesCount = nodesCountUpdated,
                                           nodeIndexShift = nodeIndexShift
                                         )
    } else /*if (
             currentNodeVal < maxNodeVal &&
               (arcsRemains.isEmpty ||
               (arcsRemains.head.arcTail >= currentNodeVal + 1 ))
           )*/ {
      //next 'nodeVal'
      //=>add 'nodeToAdd' if any to 'adjustedNodes'
      def newAdjustedNodes: Vector[ExplorableNodeWithAdjusted] =
        if (nodeToAdd.isEmpty) {
          adjustedNodes
        } else {
          adjustedNodes :+ nodeToAdd.get
        }
      val newNodeToAdd: ExplorableNodeWithAdjusted =
        ExplorableNodeWithAdjusted(
                                    //nodes(currentNodeVal),
                                    nodes(currentNodeVal + nodeIndexShift),
                                    //nodes(nodesRemains.head.node),
                                    //List()
                                    Stream.empty
                                  )
      val newCurrentNodeVal: Int =
        currentNodeVal + 1
      //nodesRemains.head.node
      //def newArcsRemains = arcsRemains

      /*recursion*/
      makeExplorableAdjacencyListFromArcs(
                                           nodes = nodes,
                                           /*nodesRemains =
                                             nodesRemains
                                                          .tail,*/
                                           currentNodeVal =
                                             currentNodeVal + 1,
                                           //newCurrentNodeVal,
                                           arcsRemains =
                                             arcsRemains,
                                           //newArcsRemains,
                                           adjustedNodes =
                                             newAdjustedNodes,
                                           nodeToAdd =
                                             Some(newNodeToAdd),
                                           maxNodeVal = maxNodeVal,
                                           //nodesCount = nodesCount + 1,
                                           nodeIndexShift = nodeIndexShift
                                         )
    }
  }

  def setNodesUnExplored(
                          minValue: Int,
                          //nodesLimit: Int
                          maxValue: Int
                          ): Array[IsExploredNode] =
    (minValue to maxValue)
    .map(IsExploredNode(_, isExplored = false))
    .toArray

  /*return 'arcs' from 'nodes' with mutable `explored` state*/
  @scala.annotation.tailrec
  def setArcsUnExplored(
                         //nodesNarcs: DirectedGraph,
                         /*unchanged*/
                         //nodes: Vector[Int],
                         /*range
                         sorted,
                          distinct,
                          1-to-1 correspondence between index & value*/
                         nodes: Vector[IsExploredNode],
                         arcsRemain: Vector[Arc],
                         /*accumulator*/
                         unExploredArcs: Vector[ArcFromNodes] =
                         Vector.empty[ArcFromNodes]
                         ): Vector[ArcFromNodes] = {
    //val DirectedGraph(nodes, arcs) = nodesNarcs
    if (arcsRemain.isEmpty) {
      /*return value*/
      unExploredArcs
    } else {
      /*val arcTail: Int = arcsRemain.head.tail
      val arcHead: Int = arcsRemain.head.head
      val nodeTailIndex: Int =
        arcTail
      val nodeHeadIndex: Int =
        arcHead*/
      /*recursion*/
      setArcsUnExplored(
                         nodes = nodes,
                         arcsRemain = arcsRemain.tail,
                         unExploredArcs =
                           /*preserve order, so 'append'*/
                           unExploredArcs :+
                             ArcFromNodes(
                                           nodes
                                           .apply(arcsRemain.head.arcTail - 1),
                                           nodes
                                           .apply(arcsRemain.head.arcHead - 1))
                       )
    }
  }

  //Breadth-First Search
  /*return connected component ?as nodes?*/
  @scala.annotation.tailrec
  def BFS(
           /*not changing, but
           it is possible to reduce it by removing arcs with explored tails*/
           graph: Vector[Arc],
           /*?as last explored?*/
           startingNode: Int,
           /*initiated with 'startingNode' as 'tail'*/
           /*contains arcs with
           explored 'tail' &
           unexplored 'head'*/
           nextArcToCheckQueue: Queue[Arc] = Queue.empty[Arc],
           /*initiated with 'startingNode'*/
           exploredNodes: Vector[Int] = Vector.empty[Int]
           ): Vector[ /*Arc*/ Int] = {
    /*A Queue is
    just like a stack
    except that
    it is `first-in-first-out` rather than
    `last-in-first-out`.*/
    //val empty = scala.collection.immutable.Queue[Int]()
    //val has1 = empty.enqueue(1)
    //val has123 = has1.enqueue(List(2, 3))
    //val (element, has23) = has123.dequeue
    if (nextArcToCheckQueue.isEmpty) {
      /*return value*/
      /*at least has starting node*/
      if (
        exploredNodes
        .nonEmpty
      //.contains(startingNode)
      ) {
        exploredNodes
      } else {
        startingNode +: exploredNodes
      }
    } else {
      val currentlyExplored: Vector[Int] =
        if (
        //exploredNodes.isEmpty ||
          exploredNodes
          .contains(startingNode)
        ) {
          exploredNodes
        } else {
          startingNode +: exploredNodes
        }
      val allArcsFromLastExplored: /*List*/ Seq[Arc] =
        graph
        //.collect(pf match {case x if x.})
        .filter(a =>
                  a.arcTail == startingNode &&
                    !exploredNodes.contains(a.arcHead))
      val (Arc(_, nextNode), dequeuedQueue): (Arc, Queue[Arc]) =
        nextArcToCheckQueue
        /*to converge and avoid endless recursion computation*/
        .dequeue
      val newQueue =
        if (allArcsFromLastExplored.isEmpty) {
          dequeuedQueue
        } else {
          dequeuedQueue
          .enqueue(allArcsFromLastExplored.toList)
        }
      /*recursion*/
      BFS(
           graph: Vector[Arc],
           startingNode = nextNode,
           /*eventually must reduce to empty*/
           newQueue,
           /*?may be must be before picking next arc from queue?*/
           currentlyExplored
         )
    }
    //Vector.empty[Int/*Arc*/]
  }

  //Breadth-First Search
  /*return connected component as nodes ranked by layers*/
  @scala.annotation.tailrec
  def layersBFS(
                 /*not changing, but
                 it is possible to reduce it by removing arcs with explored
                 tails*/
                 graph: Vector[Arc],
                 /*?as last explored?*/
                 //startingNode: RankedNode,
                 /*?for initialization only?*/
                 startingNode: Int,
                 /*initiated with 'startingNode' as 'tail'*/
                 /*contains arcs with
                 explored 'tail' &
                 unexplored 'head'*/
                 nextArcToCheckQueue: Queue[Arc] = Queue.empty[Arc],
                 /*initiated with 'startingNode'*/
                 //exploredNodes: Vector[Int] = Vector.empty[Int],
                 /*`ranked` means `explored`*/
                 rankedNodes: Vector[RankedNode] = Vector.empty[RankedNode]
                 ): Vector[RankedNode] = {
    /*
    take current 'node' =
      if `exploredNodes'.isEmpty
      then
      'startingNode' as level '0'
      else
        if 'nextNodeToCheckQueue'.nonEmpty
        then
        .dequeue
        (pick first)
        else
        end of connected component
        return `exploredNodes'
    mark it as `explored`
    (add to `explored` 'nodes')
    find all 'edges' that contain current 'node' as
    `start` or `end`
    check filtered 'edges' for 'explored' second 'node'
    add 'edges' with
    one 'node' current `explored` & second `unexplored` 'node'
    to the 'Queue'
    or
    all `unexplored` 'nodes' form 'edges' with current 'node'
    add to 'nextNodeToCheckQueue'
    then
    >>recursion<<
    * */

    if (
      rankedNodes.nonEmpty &&
        nextArcToCheckQueue.isEmpty) {
      /*return value*/
      /*at least has starting node*/
      rankedNodes
    } else /*if (nextArcToCheckQueue.nonEmpty)*/ {
      //val currentArc: Arc =
      //val (Arc(_, nextNode), dequeuedQueue): (Arc, Queue[Arc]) =
      val (currentArc, dequeuedQueue): (Arc, Queue[Arc]) =
        if (nextArcToCheckQueue.isEmpty) {
          /*?self pointed arc? at first step ?*/
          (Arc(startingNode, startingNode), Queue.empty[Arc])
        } else {
          nextArcToCheckQueue
          /*to converge and avoid endless recursion computation*/
          .dequeue
        }
      /*val (currentTail, currentHead): (Int, Int) /*Arc*/ =
        if (
        //graph.nonEmpty &&
          rankedNodes.isEmpty &&
            nextArcToCheckQueue.isEmpty
        ) {
          /*first step*/
          (startingNode, startingNode)
        } else /*if (nextArcToCheckQueue.nonEmpty)*/ {
          /*must be defined*/
          (currentArc.tail, currentArc.head)
        }*/
      /*add selected 'node' to ranked / as explored*/
      /*!only if 'node' is yet `unExplored`!*/
      val currentlyRanked: Vector[RankedNode] =
        if (
          rankedNodes.isEmpty /*||
            currentArc.tail == currentArc.head*/
        ) {
          /*for first node - only arc.tail*/
          /*?side effect?*/
          //RankedNode(currentArc.tail, 0) +: rankedNodes
          RankedNode(currentArc.arcHead, 0) +: rankedNodes
        } else if (
                 rankedNodes.nonEmpty &&
                   currentArc.arcTail == currentArc.arcHead
               ) {
          /*self loops skip*/
          /*unchanged*/
          rankedNodes
        } else /*if (
            rankedNodes.nonEmpty &&
              currentArc.tail != currentArc.head
          )*/ {
          if (rankedNodes.exists(_.node == currentArc.arcHead)) {
            /*unchanged*/
            rankedNodes
          } else {
            /*find 'tail'.rank*/
            //val tailRank: Int =
            val rankedTail: RankedNode =
              rankedNodes
              //.find(_.node == currentArc.head)
              /*`parent` 'node'*/
              .find(_.node == currentArc.arcTail)
              /*must never occur*/
              .getOrElse(RankedNode(currentArc.arcTail, 0))
            /*return value*/
            RankedNode(currentArc.arcHead, rankedTail.rank + 1) +: rankedNodes
          }
        }
      /*layer / rank of 'head' must be 'tail'.rank + 1*/
      /*!may be time consuming because of nested loop!*/
      val allArcsFromLastExplored: /*List*/ Vector[Arc] =
        graph
        .collect(
        { case a if (
          /*start with explored 'node'*/
          a.arcTail == currentArc.arcHead &&
            //!rankedNodes.unzip._1.contains(a.head)
            /*where 'arc's 'head' yet unexplored*/
            !rankedNodes.exists(_.node == a.arcHead)
          ) => a
        }
                )
      /*.filter(a =>
                a.tail == currentArc.tail &&
                  !rankedNodes
                   .contains(a.head)
             )*/
      /*(for {
        arc <- graph if arc.tail == currentArc.head
        /*contains at least one node*/
        node <- currentlyRanked if arc.head != node.node
      } yield arc)
      .distinct*/
      val newQueue =
        if (allArcsFromLastExplored.isEmpty) {
          dequeuedQueue
        } else {
          dequeuedQueue
          .enqueue(allArcsFromLastExplored.toList)
        }
      /*recursion*/
      layersBFS(
                 graph: Vector[Arc],
                 /*same & not changing*/
                 startingNode,
                 /*eventually must reduce to empty*/
                 newQueue,
                 rankedNodes = currentlyRanked
               )
    }
  }

  //Breadth-First Search
  /*return number of nodes / size of strongly connected component*/
  @scala.annotation.tailrec
  def BFS_SCC_NodesAmount(
                           graph: Vector[Arc],
                           /*only needed when called from 'findAllSCCwithBFS'*/
                           /*instead better return 'exploredNodesInSCC'*/
                           /*unExploredGraphNodes: Vector[Int]
                           = Vector.empty[Int],*/
                           /*used for initialization only*/
                           startingNode: Int,
                           nextArcToCheckQueue: Queue[Arc] = Queue.empty[Arc],
                           exploredNodesInSCC: Vector[Int] = Vector.empty[Int],
                           nodesCounter: Int = 0
                           //): Int = {
                           ): ComponentResult = {
    if (
      (nodesCounter > 0 ||
        exploredNodesInSCC.nonEmpty) &&
        nextArcToCheckQueue.isEmpty) {
      /*return value*/
      /*at least has starting node, so must be > 0*/
      //exploredNodesInSCC.length
      //nodesCounter
      ComponentResult(nodesCounter, exploredNodesInSCC)
    } else /*if (nextArcToCheckQueue.nonEmpty)*/ {
      val (currentArc, dequeuedQueue): (Arc, Queue[Arc]) =
        if (nextArcToCheckQueue.isEmpty) {
          /*?self pointed arc? at first step ?*/
          (Arc(startingNode, startingNode), Queue.empty[Arc])
        } else {
          nextArcToCheckQueue
          /*to converge and avoid endless recursion computation*/
          .dequeue
        }
      /*add selected 'node' to ranked / as explored*/
      /*!only if 'node' is yet `unExplored`!*/
      val (currentlyInSCC, newCounter): (Vector[Int], Int) =
        if (
          nodesCounter == 0 ||
            exploredNodesInSCC.isEmpty
        ) {
          /*for first node - only arc.tail*/
          (currentArc.arcHead +: exploredNodesInSCC, nodesCounter + 1)
        } else if (
                 (nodesCounter > 0 ||
                   exploredNodesInSCC.nonEmpty) &&
                   currentArc.arcTail == currentArc.arcHead
               ) {
          /*self loops skip*/
          /*unchanged*/
          (exploredNodesInSCC, nodesCounter)
        } else /*if (
            rankedNodes.nonEmpty &&
              currentArc.tail != currentArc.head
          )*/ {
          if (exploredNodesInSCC.contains(currentArc.arcHead)) {
            /*unchanged*/
            (exploredNodesInSCC, nodesCounter)
          } else {
            /*return value*/
            (currentArc.arcHead +: exploredNodesInSCC, nodesCounter + 1)
          }
        }
      /*!may be time consuming because of nested loop!*/
      val allArcsFromLastExplored: /*List*/ Vector[Arc] =
        graph
        .collect(
        { case a if (
          /*start with explored 'node'*/
          a.arcTail == currentArc.arcHead &&
            /*where 'arc's 'head' yet unexplored*/
            !exploredNodesInSCC.contains(a.arcHead)
          ) => a
        }
                )
      val newQueue =
        if (allArcsFromLastExplored.isEmpty) {
          dequeuedQueue
        } else {
          dequeuedQueue
          .enqueue(allArcsFromLastExplored.toList)
        }
      /*recursion*/
      BFS_SCC_NodesAmount(
                           graph = graph,
                           /*same & not changing*/
                           startingNode = startingNode,
                           /*eventually must reduce to empty*/
                           nextArcToCheckQueue = newQueue,
                           exploredNodesInSCC = currentlyInSCC,
                           nodesCounter = newCounter
                         )
    }
  }

  //Breadth-First Search
  /*return number of nodes / size of strongly connected component*/
  /*mark / check explored 'nodes' in-place as side effect*/
  @scala.annotation.tailrec
  def BFS_SCC_NodesAmountImproved(
                                   graph: Vector[Arc],
                                   /*only needed when called from
                                           *  */
                                   /*instead better return
                                     *  */
                                   /*unExploredGraphNodes: Vector[Int]
                                   = Vector.empty[Int],*/
                                   /*used for initialization only*/
                                   startingNode: Int,
                                   nextArcToCheckQueue: Queue[Arc] = Queue
                                                                     .empty[Arc],
                                   /*all corresponding graph 'nodes'*/
                                   graphNodes: Array[IsExploredNode],
                                   nodesCounter: Int = 0
                                   //): Int = {
                                   ): Int = {
    if (
      (nodesCounter > 0) &&
        nextArcToCheckQueue.isEmpty) {
      /*return value*/
      /*at least has starting node, so must be > 0*/
      //exploredNodesInSCC.length
      nodesCounter
    } else /*if (nextArcToCheckQueue.nonEmpty)*/ {
      val (currentArc, dequeuedQueue): (Arc, Queue[Arc]) =
        if (nextArcToCheckQueue.isEmpty) {
          /*?self pointed arc? at first step ?*/
          (Arc(startingNode, startingNode), Queue.empty[Arc])
        } else {
          nextArcToCheckQueue
          /*to converge and avoid endless recursion computation*/
          .dequeue
        }
      /*mark selected 'node' as explored*/
      /*!only if 'node' is yet `unExplored`!*/
      val newCounter: Int =
        if (
          nodesCounter == 0
        ) {
          /*for first node - only arc.tail*/
          /*side effect*/
          graphNodes(currentArc.arcHead - 1).isExplored = true
          /*return value*/
          nodesCounter + 1
        } else if (
                 (nodesCounter > 0) &&
                   currentArc.arcTail == currentArc.arcHead
               ) {
          /*self loops skip*/
          /*unchanged*/
          nodesCounter
        } else /*if (
            rankedNodes.nonEmpty &&
              currentArc.tail != currentArc.head
          )*/ {
          /*ont-to-one correspondence between 'node'.value & index*/
          if (graphNodes(currentArc.arcHead - 1).isExplored) {
            /*unchanged*/
            nodesCounter
          } else {
            /*side effect*/
            graphNodes(currentArc.arcHead - 1).isExplored = true
            /*return value*/
            nodesCounter + 1
          }
        }
      /*!may be time consuming because of nested loop!*/
      val allArcsFromLastExplored: /*List*/ Vector[Arc] =
        graph
        .collect(
        { case a if (
          /*start with explored 'node'*/
          a.arcTail == currentArc.arcHead &&
            /*where 'arc's 'head' yet unexplored*/
            !graphNodes(a.arcHead - 1).isExplored
          ) => a
        }
                )
      val newQueue =
        if (allArcsFromLastExplored.isEmpty) {
          dequeuedQueue
        } else {
          dequeuedQueue
          .enqueue(allArcsFromLastExplored.toList)
        }
      /*recursion*/
      BFS_SCC_NodesAmountImproved(
                                   graph = graph,
                                   /*same & not changing*/
                                   startingNode = startingNode,
                                   /*eventually must reduce to empty*/
                                   nextArcToCheckQueue = newQueue,
                                   graphNodes = graphNodes,
                                   nodesCounter = newCounter
                                 )
    }
  }

  //Breadth-First Search
  /*return number of nodes / size of strongly connected component*/
  /*mark / check explored 'nodes' in-place as side effect*/
  @scala.annotation.tailrec
  def BFS_SCC_NodesAmountOptimized(
                                    graph: Vector[ArcFromNodes],
                                    /*used for initialization only*/
                                    startingNode: Int,
                                    nextArcToCheckQueue: Queue[ArcFromNodes] =
                                    Queue
                                    .empty[ArcFromNodes],
                                    /*all corresponding graph 'nodes'*/
                                    nodesCounter: Int = 0
                                    //): Int = {
                                    ): Int = {
    if (
      (nodesCounter > 0) &&
        nextArcToCheckQueue.isEmpty) {
      /*return value*/
      /*at least has starting node, so must be > 0*/
      //exploredNodesInSCC.length
      nodesCounter
    } else /*if (nextArcToCheckQueue.nonEmpty)*/ {
      /*cases:
      >first step
      >>startingNode.isSink
      >>startingNode.nonSink
      >nextArcToCheckQueue.nonEmpty
      * */
      val (currentArc, dequeuedQueue): (ArcFromNodes, Queue[ArcFromNodes]) =
        if (nextArcToCheckQueue.isEmpty) {
          /*>>?self pointed arc? at first step ?<<*/
          /*'node' may be 'tail', so point to 'head' or
          * it may be 'head' only, so 'sink' & point nowhere
          * then CC has exactly one 'node'
          * also
          * may point to itself
          * */
          val searchForArc: Option[ArcFromNodes] =
            graph
            /*must be at least one 'arc'*/
            .find(_.arcTail.node == startingNode)
          //.get
          if (searchForArc.isEmpty) {
            /*'head' only or 'sink'*/
            /*must exit at next iteration*/
            (graph
             /*must be at least one 'arc'*/
             .find(_.arcHead.node == startingNode)
             .get,
              Queue.empty[ArcFromNodes])
          } else {
            (searchForArc.get,
              Queue.empty[ArcFromNodes])
          }
        } else {
          nextArcToCheckQueue
          /*to converge and avoid endless recursion computation*/
          .dequeue
        }
      /*mark selected 'node' as explored*/
      /*!only if 'node' is yet `unExplored`!*/
      val newCounter: Int =
        if (
          nodesCounter == 0
        ) {
          /*for the first 'arc'*/
          if (
            currentArc.arcTail.node != startingNode &&
              currentArc.arcHead.node == startingNode
          ) {
            /*sink*/
            /*side effect*/
            currentArc.arcHead.isExplored = true
            /*return value*/
            nodesCounter + 1
          } else if (
                   currentArc.arcTail.node == startingNode &&
                     currentArc.arcHead.node == startingNode
                 ) {
            /*self reference*/
            /*may be sink*/
            /*side effect*/
            currentArc.arcHead.isExplored = true
            /*return value*/
            nodesCounter + 1
          } else /*if (
                                                currentArc.arcHead.node !=
                                                startingNode
                                            )*/ {
            /*not 'sink', both 'nodes' in 'SCC'*/
            /*side effect*/
            currentArc.arcTail.isExplored = true
            //currentArc.arcHead.isExplored = true
            /*return value*/
            nodesCounter + 1 //2
          }
        } else if (
                 (nodesCounter > 0) &&
                   currentArc.arcTail.node == currentArc.arcHead.node
               ) {
          /*self loops skip*/
          /*unchanged*/
          nodesCounter
        } else /*if (
            rankedNodes.nonEmpty &&
              currentArc.tail != currentArc.head
          )*/ {
          if (currentArc.arcHead.isExplored) {
            /*unchanged*/
            nodesCounter
          } else {
            /*side effect*/
            currentArc.arcHead.isExplored = true
            /*return value*/
            nodesCounter + 1
          }
        }
      /*!may be time consuming because of nested loop!*/
      val allArcsFromLastExplored: Vector[ArcFromNodes] =
        collectInducedArcs(
                            arcs = graph,
                            /*for first step must be 'arcTail'*/
                            tailValue =
                              if (nodesCounter == 0) {
                                currentArc.arcTail.node
                              } else {
                                currentArc.arcHead.node
                              }
                          )
      val newQueue =
        if (allArcsFromLastExplored.isEmpty) {
          dequeuedQueue
        } else {
          dequeuedQueue
          /*time consuming conversion*/
          //.enqueue(allArcsFromLastExplored.toList)
          .enqueue(allArcsFromLastExplored)
        }
      /*recursion*/
      BFS_SCC_NodesAmountOptimized(
                                    graph = graph,
                                    /*same & not changing*/
                                    startingNode = startingNode,
                                    /*eventually must reduce to empty*/
                                    nextArcToCheckQueue = newQueue,
                                    nodesCounter = newCounter
                                  )
    }
  }

  //Breadth-First Search
  /*return sequence of number of nodes / size of all strongly connected
  components*/
  /*too slow on big input*/
  @scala.annotation.tailrec
  def findAllSCCwithBFS(
                         /*may be worth it to be shrinked in each iteration*/
                         graph: Vector[Arc],
                         /*all corresponding graph 'nodes'*/
                         unExploredGraphNodes: Vector[Int],
                         exploredGraphNodes: Vector[Int] =
                         Vector.empty[Int],
                         connectedComponents: Vector[Int] =
                         Vector.empty[Int]
                         ): Seq[Int] = {
    if (
      unExploredGraphNodes.isEmpty) {
      /*return value*/
      connectedComponents
    } else /*if (unExploredGraphNodes.nonEmpty)*/ {
      /*time consuming operation*/
      if (exploredGraphNodes.contains(unExploredGraphNodes.head)) {
        /*skip to next node*/
        /*recursion*/
        findAllSCCwithBFS(
                           graph: Vector[Arc],
                           /*eventually must reduce to empty*/
                           unExploredGraphNodes.tail,
                           exploredGraphNodes,
                           connectedComponents
                         )
      } else {
        /*have new unexplored 'node' not in explored SCC*/
        val ComponentResult(
        exploredConnectedComponent,
        newExploredNodes
                           ): ComponentResult =
          BFS_SCC_NodesAmount(
                               graph = graph,
                               startingNode = unExploredGraphNodes.head
                             )
        val updatedConnectedComponents: Vector[Int] =
          exploredConnectedComponent +: connectedComponents
        /*time consuming operation*/
        val updatedExploredNodes: Vector[Int] =
          exploredGraphNodes.union(newExploredNodes)
        /*recursion*/
        findAllSCCwithBFS(
                           graph: Vector[Arc],
                           /*eventually must reduce to empty*/
                           unExploredGraphNodes.tail,
                           updatedExploredNodes,
                           updatedConnectedComponents
                         )
      }
    }
  }

  //Breadth-First Search
  /*return sequence of number of nodes / size of all strongly connected
  components*/
  /*do all explored checks in-place in special array*/
  @scala.annotation.tailrec
  def findAllSCCwithBFSImproved(
                                 /*may be worth it to be shrinked in each
                                 iteration*/
                                 graph: Vector[Arc],
                                 /*all corresponding graph 'nodes'*/
                                 graphNodes: Array[IsExploredNode],
                                 /*as 'nodes' are range from '1' to
                                    *  */
                                 nodesLimit: Int,
                                 currentNodeIndex: Int = 0,
                                 connectedComponents: Seq[Int] =
                                 Seq.empty[Int]
                                 ): Seq[Int] = {
    if (
      currentNodeIndex >= nodesLimit) {
      /*return value*/
      connectedComponents
    } else /*if (unExploredGraphNodes.nonEmpty)*/ {
      /*time consuming operation*/
      if (graphNodes(currentNodeIndex).isExplored) {
        /*skip to next node*/
        /*recursion*/
        findAllSCCwithBFSImproved(
                                   graph: Vector[Arc],
                                   graphNodes = graphNodes,
                                   nodesLimit = nodesLimit,
                                   /*eventually must exceed limit*/
                                   currentNodeIndex + 1,
                                   connectedComponents
                                 )
      } else {
        /*have new unexplored 'node' not in explored SCC*/
        val exploredConnectedComponent: Int =
          BFS_SCC_NodesAmountImproved(
                                       graph = graph,
                                       startingNode =
                                         /*must be within bounds*/
                                         graphNodes(currentNodeIndex).node,
                                       graphNodes = graphNodes
                                     )
        val updatedConnectedComponents: Seq[Int] =
          exploredConnectedComponent +: connectedComponents
        /*recursion*/
        findAllSCCwithBFSImproved(
                                   graph: Vector[Arc],
                                   graphNodes = graphNodes,
                                   nodesLimit = nodesLimit,
                                   /*eventually must must exceed limit*/
                                   currentNodeIndex + 1,
                                   connectedComponents =
                                     updatedConnectedComponents
                                 )
      }
    }
  }

  //Breadth-First Search
  /*return sequence of number of nodes / size of all connected
  components*/
  /*do all explored checks in-place in special array*/
  @scala.annotation.tailrec
  def findAllCCwithBFSOptimized(
                                 /*may be worth it to be shrinked in each
                                 iteration*/
                                 graph: Vector[ArcFromNodes],
                                 /*all corresponding graph 'nodes'*/
                                 /*`lookUp` 'apply' for 'Array'
                                 * 'C'	The operation takes (fast) constant
                                 * time.
                                 * vs. 'eC' for 'Vector'*/
                                 graphNodes: Array[IsExploredNode],
                                 /*as 'nodes' are range from '1' to
                                    *  */
                                 nodesLimit: Int,
                                 currentNodeIndex: Int = 0,
                                 connectedComponents: Seq[Int] =
                                 Seq.empty[Int]
                                 ): Seq[Int] = {
    if (
      currentNodeIndex >= nodesLimit) {
      /*return value*/
      connectedComponents
    } else /*if (unExploredGraphNodes.nonEmpty)*/ {
      /*time consuming operation*/
      if (graphNodes(currentNodeIndex).isExplored) {
        /*skip to next node*/
        /*recursion*/
        findAllCCwithBFSOptimized(
                                   graph = graph,
                                   graphNodes = graphNodes,
                                   nodesLimit = nodesLimit,
                                   /*eventually must exceed limit*/
                                   currentNodeIndex + 1,
                                   connectedComponents
                                 )
      } else {
        /*have new unexplored 'node' not in explored SCC*/
        val exploredConnectedComponent: Int =
          BFS_SCC_NodesAmountOptimized(
                                        graph = graph,
                                        startingNode =
                                          /*must be within bounds*/
                                          graphNodes(currentNodeIndex).node /*,
                                       nodesCounter = 0*/
                                      )
        val updatedConnectedComponents: Seq[Int] =
          exploredConnectedComponent +: connectedComponents
        /*recursion*/
        findAllCCwithBFSOptimized(
                                   graph = graph,
                                   graphNodes = graphNodes,
                                   nodesLimit = nodesLimit,
                                   /*eventually must must exceed limit*/
                                   currentNodeIndex + 1,
                                   connectedComponents =
                                     updatedConnectedComponents
                                 )
      }
    }
  }

  //Breadth-First Search
  /*return all connected components in graph as sequences of nodes*/
  @scala.annotation.tailrec
  def connectedComponentsBFS(
                              /*not changing, but
                              it is possible to reduce it by removing arcs
                              with explored tails*/
                              graphNodes: Vector[Int],
                              graphArcs: Vector[Arc],
                              graphComponents: Vector[Vector[Int]] = Vector
                                                                     .empty[Vector[Int]]
                              ): Vector[Vector[Int]] = {
    if (
      graphArcs.isEmpty ||
        graphNodes.isEmpty
    ) {
      /*return value*/
      /*? at least has starting node ?*/
      graphComponents
    } else {
      /*
      for each remaining non explored node in graph
      run BFS
      add result to 'graphComponents' accumulator
       */
      /*recursion*/
      connectedComponentsBFS(
                              graphNodes,
                              graphArcs,
                              graphComponents
                            )
    }
  }

  /*by Marimuthu Madasamy
  with some modification by me Alex
  * */
  def traverse(
                graph: Map[Int, Set[Int]],
                start: Int): List[Int] = {
    /*def childrenNotVisited(parent: Int, visited: List[Int]) =
      graph(parent) filter (x => !visited.contains(x))*/

    @annotation.tailrec
    def loop(
              /*`nodes` remain to check*/
              stack: Set[Int],
              /*to maintain / preserve order*/
              visited: List[Int],
              /*to check condition*/
              visitedSet: Set[Int] =
              Set.empty
              ): List[Int] = {
      if (stack isEmpty) {
        /*return value*/
        visited
      }
      else {
        val visitedSetUpdated: Set[Int] =
          visitedSet + stack.head
        /*recursion*/
        //assume(graph.isDefinedAt(start))
        //assume(graph.isDefinedAt(stack.head))
        /*reduce 'stack' by 'head' to converge eventually*/
        loop(
              //childrenNotVisited(stack.head, visited) ++ stack.tail,
              graph(stack.head)
              .diff(
                  //visitedSet
                    visitedSetUpdated
                   )
              .union(stack.tail),
              stack.head :: visited,
              /*to keep track on `visited` & simplify `contains` check*/
              //visitedSet + stack.head
              visitedSetUpdated
            )
      }
    }
    /*initialization*/
    /*return value*/
    loop(
          Set(start),
          //Nil
          List.empty
        )
    /*because 'visited' formed by `prepend`*/
    .reverse
  }

  // run 'DFS' in `digraph` 'G'
  // from vertex 'v' and
  // compute `preorder`/`postorder`
  //@scala.annotation.tailrec
  def dfs(
           graph: Map[Int, NodeMapValFieldsStatic],
           startFromNodeKey: Int,
           globalExploredNodesSet:
           BitSet =
           BitSet.empty,
           dfsResults: DFSResults =
           DFSResults(
                       preOrder = List.empty,
                       postOrder = List.empty,
                       reachable = 0
                     ),
           globalPreQueue: scala.collection.immutable.Queue[Int] =
           Queue.empty[Int],
           adjustedNodesKeysRemains: Set[Int] =
           Set.empty
           ): DFSResults = {
    //): Unit = {
    //marked[v] = true
    /*state*/
    var localExploredNodesSet:
    BitSet =
    //explored
      BitSet.empty
    /*state*/
    var localDFS_Results: DFSResults = dfsResults
    var localPreQueue: scala.collection.immutable.Queue[Int] =
    //Queue.empty[Int]
      globalPreQueue

    /*check for 'node' existence*/
    val currentNodeValue: Option[NodeMapValFieldsStatic] =
      graph
      .get(startFromNodeKey)
    if (currentNodeValue.isEmpty) {
      /*'key' no in current graph*/
      /*exit with unchanged values*/
      localDFS_Results
    } else /*if (currentNodeValue.isDefined)*/ {
      /*mark as `explored`*/
      localExploredNodesSet = localExploredNodesSet + startFromNodeKey
      val adjustedNodesKeys: Set[Int] =
        currentNodeValue.get.adjustedNodes
      /*set order number for 'node'*/
      //pre[v] = preCounter++
      /*add / append order number to 'preorder'*/
      //preorder.enqueue(v)
      /*`append` for 'Queue' takes constant time*/
      localPreQueue = localPreQueue.enqueue(startFromNodeKey)
      localDFS_Results =
        DFSResults(
                    /*`append` is time consuming for 'List' operation*/
                    localDFS_Results.preOrder :+ startFromNodeKey,
                    localDFS_Results.postOrder,
                    localDFS_Results.reachable + 1
                  )
      //for (int w : G.adj(v)) {
      /*check all available adjusted 'nodes' recursively */
      for (nextAdjustedNodeKey <- adjustedNodesKeys) {
        if (
        //!marked[w]
          !localExploredNodesSet
           .contains(nextAdjustedNodeKey)
        ) {
          /*recursion*/
          //dfs(G, w)
          dfs(graph = graph,
              startFromNodeKey = nextAdjustedNodeKey)
        } else /*if (nextAdjustedNode.isExplored)*/ {
          /*go to the 'nextAdjustedNode'*/
          /*same results as before*/
        }
      }
      /*return value*/
      /*after 'for' `loop` content of 'postorder' may change
      within 'for' recursive calls
       */
      /*add order number to 'postorder'*/
      //postorder.enqueue(v)
      /*set order number for 'node'*/
      //post[v] = postCounter++
      DFSResults(
                  /*`append` is time consuming for 'List' operation*/
                  localDFS_Results.preOrder,
                  localDFS_Results.postOrder :+ startFromNodeKey,
                  localDFS_Results.reachable
                )
    }
  }

  /*Depth-first search (DFS)*/
  //A recursive implementation of DFS
  //Input: A graph 'G' and a (starting) vertex 'v' of 'G'
  //Output: All `vertices` reachable from 'v', labeled as discovered
  def preOrderDFS(
                   /*G*/ graph: Vector[ExplorableNodeWithAdjusted],
                   /*start node*/
                   v: Int,
                   /*nodeToCheck: Option[ExplorableNodeWithAdjusted] =
                   None,*/
                   /*accum*/
                   /*exploredNodes: List[IsExploredNode] =
                   List.empty
                   ): List[IsExploredNode] = {*/
                   exploredNodes: Stream[IsExploredNode] =
                   Stream.empty,
                   nodeIndexShift: Int = -1
                   ): Stream[IsExploredNode] = {
    @scala.annotation.tailrec
    def innerLoop(
                   /*adjacentEdges: List[IsExploredNode],
                   exploredNodes: List[IsExploredNode]
                   ): List[IsExploredNode] = {*/
                   adjacentEdges: Stream[IsExploredNode],
                   exploredNodes: Stream[IsExploredNode]
                   ): Stream[IsExploredNode] = {
      if (adjacentEdges.isEmpty) {
        /*return value*/
        exploredNodes
      } else {
        //val exploredNodesUpdated: List[IsExploredNode] =
        val exploredNodesUpdated: Stream[IsExploredNode] =
          if (adjacentEdges.head.isExplored) {
            /*skip current 'node', check next*/
            /*same value*/
            exploredNodes
          } else /*if (!adjacentEdges.head.isExplored)*/ {
            /*outer recursion*/
            preOrderDFS(
                         graph = graph,
                         v = adjacentEdges.head.node,
                         //nodeToCheck = Some(adjacentEdges.head),
                         exploredNodes =
                           exploredNodes
                       )
          }
        /*recursion*/
        innerLoop(
                   adjacentEdges = adjacentEdges.tail,
                   exploredNodes = exploredNodesUpdated
                 )
      }
    }
    //label 'v' as `discovered`
    val starNode: ExplorableNodeWithAdjusted =
      graph(v + nodeIndexShift)
    /*side effect*/
    /*must be added to output only once*/
    starNode.node.isExplored = true
    /*for all edges from v to w in G.adjacentEdges(v) do
        if vertex w is not labeled as discovered then
              recursively call DFS(G,w)*/
    /*initialization*/
    innerLoop(
               adjacentEdges =
                 starNode.adjustedNodes,
               exploredNodes =
                 exploredNodes :+ starNode.node
             )
  }

  /*TODO obviously it must be done in nonFunctional JAVA way with 'Array'*/
  /*Depth-first search (DFS)*/
  //A recursive implementation of DFS
  //Input: A graph 'G' and a (starting) vertex 'v' of 'G'
  //Output: All `vertices` reachable from 'v', labeled as discovered
  /*up to graph 'nodes' size*/
  /*how to refactor this as 'tailrec' ?*/
  def postOrderDFS_Nodes(
                          /*G*/ graph: => Vector[ExplorableNodeWithAdjusted],
                          /*start node*/
                          v: Int,
                          /*nodeToCheck: Option[ExplorableNodeWithAdjusted] =
                          None,*/
                          /*accum*/
                          /*exploredNodes: List[IsExploredNode] =
                          List.empty*/
                          exploredNodes: => Stream[IsExploredNode] =
                          Stream.empty,
                          nodeIndexShift: Int = -1
                          ): Stream[IsExploredNode] = {
    //): List[IsExploredNode] = {
    //assume(graph.nonEmpty, s"graph must be '.nonEmpty'")

    //label 'v' as `discovered`
    val starNode: ExplorableNodeWithAdjusted =
      graph(v + nodeIndexShift)

    @scala.annotation.tailrec
    def innerLoop(
                   /*adjacentEdges: List[IsExploredNode],
                   exploredNodes: List[IsExploredNode]
                   ): List[IsExploredNode] = {*/
                   adjacentEdges: => Stream[IsExploredNode],
                   exploredNodes: => Stream[IsExploredNode]
                   ): Stream[IsExploredNode] = {
      if (adjacentEdges.isEmpty) {
        /*return value*/
        /*same value for 'preOrder'*/
        //exploredNodes
        /*?prepend? or 'append' then reverse ?*/
        /*return right order, but
        possible cause of 'StackOverflowError'
         */
        exploredNodes :+ starNode.node
        /*affect program flow in a strange way
        * and still throws 'StackOverflowError'*/
        ////starNode.node +: exploredNodes
      } else {
        //val exploredNodesUpdated: List[IsExploredNode] =
        /*because 'val' ensures that `memorizaion` occurs*/
        def exploredNodesUpdated: Stream[IsExploredNode] =
          if (adjacentEdges.head.isExplored) {
            /*skip current 'node', check next*/
            /*same value for 'preOrder'*/
            exploredNodes
          } else /*if (!adjacentEdges.head.isExplored)*/ {
            /*outer recursion*/
            postOrderDFS_Nodes(
                                graph = graph,
                                v = adjacentEdges.head.node,
                                //nodeToCheck = Some(adjacentEdges.head),
                                exploredNodes =
                                  /*same value for 'preOrder'*/
                                  exploredNodes
                                //exploredNodes :+ adjacentEdges.head
                                //exploredNodes :+ starNode.node
                              )
          }
        /*recursion*/
        innerLoop(
                   adjacentEdges = adjacentEdges.tail,
                   exploredNodes = exploredNodesUpdated
                 )
      }
    }

    /*side effect*/
    /*must be added to output only once*/
    starNode.node.isExplored = true
    /*for all edges from v to w in G.adjacentEdges(v) do
        if vertex w is not labeled as discovered then
              recursively call DFS(G,w)*/
    /*initialization*/
    innerLoop(
               adjacentEdges =
                 starNode.adjustedNodes,
               /*for postOrder be added later within 'innerLoop' check*/
               exploredNodes =
                 exploredNodes
               //exploredNodes :+ starNode.node
             )
  }

  /*Depth-first search (DFS)*/
  //A recursive implementation of DFS
  //Input: A graph 'G' and a (starting) vertex 'v' of 'G'
  //Output: All `nodes.values` reachable from 'v', labeled as discovered
  /*how to refactor this as 'tailrec' ?*/
  def postOrderDFS_ValOnly(
                            /*G*/ graph: => Vector[ExplorableNodeWithAdjusted],
                            /*start node*/
                            v: Int,
                            /*nodeToCheck: Option[ExplorableNodeWithAdjusted] =
                            None,*/
                            /*accum*/
                            exploredNodes: => List[Int] =
                            List.empty,
                            /*exploredNodes: => Stream[IsExploredNode] =
                            Stream.empty,*/
                            nodeIndexShift: Int = -1
                            //): Stream[Int] = {
                            ): List[Int] = {
    //assume(graph.nonEmpty, s"graph must be '.nonEmpty'")

    //label 'v' as `discovered`
    val starNode: ExplorableNodeWithAdjusted =
      graph(v + nodeIndexShift)

    @scala.annotation.tailrec
    def innerLoop(
                   adjacentEdges: => List[IsExploredNode],
                   exploredNodes: => List[Int]
                   ): List[Int] = {
      /*adjacentEdges: => Stream[IsExploredNode],
      exploredNodes: => Stream[Int]
      ): Stream[Int] = {*/
      if (adjacentEdges.isEmpty) {
        /*return value*/
        /*same value for 'preOrder'*/
        //exploredNodes
        /*?prepend? or 'append' then reverse ?*/
        exploredNodes :+ starNode.node.node
      } else {
        //val exploredNodesUpdated: List[Int] =
        def exploredNodesUpdated: List[Int] =
        /*because 'val' ensures that `memorization` occurs*/
        //def exploredNodesUpdated: Stream[Int] =
          if (adjacentEdges.head.isExplored) {
            /*skip current 'node', check next*/
            /*same value for 'preOrder'*/
            exploredNodes
          } else /*if (!adjacentEdges.head.isExplored)*/ {
            /*outer recursion*/
            postOrderDFS_ValOnly(
                                  graph = graph,
                                  v = adjacentEdges.head.node,
                                  //nodeToCheck = Some(adjacentEdges.head),
                                  exploredNodes =
                                    /*same value for 'preOrder'*/
                                    exploredNodes
                                  //exploredNodes :+ adjacentEdges.head
                                  //exploredNodes :+ starNode.node
                                )
          }
        /*recursion*/
        innerLoop(
                   adjacentEdges = adjacentEdges.tail,
                   exploredNodes = exploredNodesUpdated
                 )
      }
    }

    /*side effect*/
    /*must be added to output only once*/
    starNode.node.isExplored = true
    /*for all edges from v to w in G.adjacentEdges(v) do
        if vertex w is not labeled as discovered then
              recursively call DFS(G,w)*/
    /*initialization*/
    innerLoop(
               adjacentEdges =
                 starNode
                 .adjustedNodes
                 .view
                 .toList,
               /*for postOrder be added later within 'innerLoop' check*/
               exploredNodes =
                 exploredNodes
               //exploredNodes :+ starNode.node
             )
  }

  /*Depth-first search (DFS)*/
  //A recursive implementation of DFS
  //Input: A graph 'G' and a (starting) vertex (node) 'v' of 'G'
  //Output: number of `nodes` reachable from 'v', labeled as discovered
  def maxPostOrderDFS(
                       graph: => Vector[ExplorableNodeWithAdjusted],
                       /*start node*/
                       v: Int,
                       exploredNodes: Int =
                       0,
                       nodeIndexShift: Int = -1
                       ): Int = {
    //assume(graph.nonEmpty, s"graph must be '.nonEmpty'")

    //label 'v' as `discovered`
    val starNode: ExplorableNodeWithAdjusted =
      graph(v + nodeIndexShift)

    @scala.annotation.tailrec
    def innerLoop(
                   //adjacentEdges: List[IsExploredNode],
                   adjacentEdges: => Stream[IsExploredNode],
                   /*accum*/
                   exploredNodesResult: Int
                   ): Int = {
      if (adjacentEdges.isEmpty) {
        /*return value*/
        /*same value for 'preOrder'*/
        //exploredNodesResult
        exploredNodesResult + 1
      } else {
        def exploredNodesUpdated: Int =
          if (adjacentEdges.head.isExplored) {
            /*skip current 'node', check next*/
            /*same value for 'preOrder'*/
            exploredNodesResult
          } else /*if (!adjacentEdges.head.isExplored)*/ {
            /*outer recursion*/
            maxPostOrderDFS(
                             graph = graph,
                             v = adjacentEdges.head.node,
                             exploredNodes =
                               /*same value for 'preOrder'*/
                               exploredNodesResult
                           )
          }
        /*recursion*/
        innerLoop(
                   adjacentEdges = adjacentEdges.tail,
                   exploredNodesResult = exploredNodesUpdated
                 )
      }
    }

    /*side effect*/
    /*must be added to output only once*/
    starNode.node.isExplored = true
    /*initialization*/
    innerLoop(
               adjacentEdges =
                 starNode.adjustedNodes,
               /*for postOrder be added later within 'innerLoop' check*/
               exploredNodesResult =
                 exploredNodes
             )
  }

  /*Depth-first search (DFS)*/
  def preAndPostOrderDFS(
                          graph: Vector[ExplorableNodeWithAdjusted],
                          /*start node*/
                          v: Int,
                          /*accum*/
                          /*exploredNodesPre: List[IsExploredNode] =
                          List.empty,
                          exploredNodesPost: List[IsExploredNode] =
                          List.empty,*/
                          exploredNodesPre: Stream[IsExploredNode] =
                          Stream.empty,
                          exploredNodesPost: Stream[IsExploredNode] =
                          Stream.empty,
                          //nodesValuesZeroBased: Boolean = false
                          nodeIndexShift: Int = -1
                          ): DepthFirstSearchResult = {
    //label 'v' as `discovered`
    val starNode: ExplorableNodeWithAdjusted =
    /*may be out of bound
    when nodes values zero based and,
    so equal to indices*/
    //if (nodesValuesZeroBased) {
    //graph(v)
      graph(v + nodeIndexShift)
    /*} else {
      graph(v - 1)
    }*/

    @scala.annotation.tailrec
    def innerLoop(
                   /*adjacentEdges: List[IsExploredNode],
                   preExploredNodes: List[IsExploredNode],
                   postExploredNodes: List[IsExploredNode]*/
                   adjacentEdges: Stream[IsExploredNode],
                   preExploredNodes: Stream[IsExploredNode],
                   postExploredNodes: Stream[IsExploredNode]
                   ): DepthFirstSearchResult = {
      if (adjacentEdges.isEmpty) {
        /*return value*/
        DepthFirstSearchResult(
                                /*same value for 'preOrder'*/
                                preExploredNodes,
                                postExploredNodes :+ starNode.node
                              )
      } else {
        val DepthFirstSearchResult(preExploredNodesUpdated,
                                   postExploredNodesUpdated):
        DepthFirstSearchResult =
          if (adjacentEdges.head.isExplored) {
            /*skip current 'node', check next*/
            DepthFirstSearchResult(
                                    /*same value for 'preOrder'*/
                                    preExploredNodes,
                                    postExploredNodes
                                  )
          } else /*if (!adjacentEdges.head.isExplored)*/ {
            /*outer recursion*/
            preAndPostOrderDFS(
                                graph = graph,
                                v = adjacentEdges.head.node,
                                //nodeToCheck = Some(adjacentEdges.head),
                                exploredNodesPre =
                                  /*same value for 'preOrder'*/
                                  preExploredNodes,
                                //exploredNodes :+ adjacentEdges.head
                                //exploredNodes :+ starNode.node
                                exploredNodesPost =
                                  postExploredNodes //,
                                //nodesValuesZeroBased = nodesValuesZeroBased
                              )
          }
        /*recursion*/
        innerLoop(
                   adjacentEdges = adjacentEdges.tail,
                   preExploredNodes = preExploredNodesUpdated,
                   postExploredNodes =
                     postExploredNodesUpdated
                 )
      }
    }

    /*side effect*/
    /*must be added to output only once*/
    starNode.node.isExplored = true
    /*for all edges from v to w in G.adjacentEdges(v) do
        if vertex w is not labeled as discovered then
              recursively call DFS(G,w)*/
    /*initialization*/
    innerLoop(
               adjacentEdges =
                 starNode.adjustedNodes,
               /*for `postOrder` be added later within 'innerLoop' check*/
               preExploredNodes =
                 exploredNodesPre :+ starNode.node,
               postExploredNodes =
                 exploredNodesPost
             )
  }

  /*Depth-first search (DFS)*/
  def pre_PostOrderDFS_Map(
                            graph: Map[Int, NodeMapValFieldsStatic],
                            /*start node*/
                            startNode: Int,
                            /*accum*/
                            exploredNodesPre:
                            List[Int] =
                            List.empty,
                            /*Stream[Int] =
                            Stream.empty,*/
                            exploredNodesPost:
                            /*Stream[Int] =
                            Stream.empty,*/
                            List[Int] =
                            List.empty,
                            /*may be redundant*/
                            exploredNodesSet:
                            Set[Int] =
                            Set.empty
                            /*BitSet =
                          BitSet.empty*/
                            ): DFSResultInt = {
    //label 'v' as `discovered`
    val startNodeGet: Option[NodeMapValFieldsStatic] =
      graph
      /*?fails?*/
      .get(startNode)

    @scala.annotation.tailrec
    def innerLoop(
                   adjacentEdges: Set[Int],
                   preExploredNodes:
                   List[Int],
                   //Stream[Int],
                   postExploredNodes:
                   List[Int],
                   //Stream[Int],
                   exploredNodesSet:
                   Set[Int]
                   //BitSet
                   //exploredNodesSet: => BitSet
                   ): DFSResultInt = {
      if (adjacentEdges.isEmpty) {
        /*return value*/
        DFSResultInt(
                      /*same value for 'preOrder'*/
                      preExploredNodes,
                      /*'append', then 'reverse' somewhere later*/
                      if (startNodeGet.isEmpty) {
                        postExploredNodes
                      } else {
                        startNode +: postExploredNodes
                      }
                    )
      } else /*if (adjacentEdges.nonEmpty)*/ {
        /*must have 'head'*/
        /*?fails?*/
        val adjacentEdgesHead: Int =
        //def adjacentEdgesHead: Int =
          adjacentEdges
          //.toStream
          //.toList
          //.toTraversable
          //.toIterable
          .head
        /*.toIterator
        .next()*/
        //adjacentEdges.take(1)
        val DFSResultInt(
        preExploredNodesUpdated,
        postExploredNodesUpdated):
        DFSResultInt =
          if (
          //adjacentEdges.head.isExplored
            exploredNodesSet.contains(adjacentEdgesHead)
          ) {
            /*skip current 'node', check next*/
            DFSResultInt(
                          /*same value for 'preOrder'*/
                          preExploredNodes,
                          postExploredNodes
                        )
          } else /*if (!adjacentEdges.head.isExplored)*/ {
            /*outer recursion*/
            pre_PostOrderDFS_Map(
                                  graph = graph,
                                  startNode = adjacentEdgesHead,
                                  exploredNodesPre =
                                    /*same value for 'preOrder'*/
                                    preExploredNodes,
                                  exploredNodesPost =
                                    postExploredNodes
                                )
          }
        /*recursion*/
        innerLoop(
                   adjacentEdges =
                     adjacentEdges - adjacentEdgesHead,
                   //adjacentEdges.tail,
                   preExploredNodes = preExploredNodesUpdated,
                   postExploredNodes =
                     postExploredNodesUpdated,
                   exploredNodesSet
                 )
      }
    }

    /*?side effect?*/
    /*must be added to output only once*/
    val adjustedNodesForStarted: Set[Int] =
      if (startNodeGet.isEmpty) {
        /*!something does wrong!*/
        Set.empty
      } else {
        //startNodeGet.get.isExplored = true
        startNodeGet.get.adjustedNodes
      }
    /*initialization*/
    innerLoop(
               adjacentEdges =
                 adjustedNodesForStarted,
               /*for `postOrder` be added later within 'innerLoop' check*/
               preExploredNodes =
                 /*'append', then 'reverse' somewhere later*/
                 startNode +: exploredNodesPre,
               postExploredNodes =
                 exploredNodesPost,
               exploredNodesSet
             )
  }

  /*Depth-first search (DFS)*/
  /*must skip not existing starting 'node' values*/
  def pre_PostOrderDFS_ver2(
                             graph: Map[Int, NodeMapValFieldsStatic],
                             /*start node*/
                             startNode: Int
                             ): BitSet = {
    //): DFSResultInt = {
    /*state*/
    var exploredNodesSet:
    /*Set[Int] =
      Set.empty*/
    BitSet =
      BitSet.empty
    /*accum*/
    var exploredNodesPre:
    List[Int] =
      List.empty
    /*Stream[Int] =
    Stream.empty,*/
    var exploredNodesPost:
    /*Stream[Int] =
    Stream.empty,*/
    List[Int] =
      List.empty
    var nodesStackList: List[Int] =
      List(startNode)
    /*val startNodeGet: Option[NodeMapValFieldsStatic] =
      graph
        /*?fails?*/
      .get(startNode)*/

    /*infinite loop*/
    @scala.annotation.tailrec
    def innerLoop(
                   adjacentEdges: Set[Int] =
                   Set.empty
                   ): BitSet = {
      //): Unit = {
      if (nodesStackList.isEmpty) {
        /*return value*/
        /*DFSResultInt(
                      /*same value for 'preOrder'*/
                      preExploredNodes,
                      /*'append', then 'reverse' somewhere later*/
                      if (startNodeGet.isEmpty) {
                        postExploredNodes
                      } else {
                        startNode +: postExploredNodes
                      }
                    )*/
        exploredNodesSet
      } else /*if (nodesStackList.nonEmpty)*/ {
        val nextNodeFromStackKey: Int =
          nodesStackList.head

        /*side effect*/
        /*reduce loop condition to converge*/
        nodesStackList = nodesStackList.tail

        if (
        //nextNodeFromStack.isExplored
          exploredNodesSet
          .contains(nextNodeFromStackKey)
        ) {
          /*to recursion*/
          /*skip current 'node', check next*/
          /*recursion*/
          innerLoop()
        } else /*if (nextNodeFromStack.notExplored)*/ {
          /*side effect*/
          /*actual result*/
          exploredNodesSet =
            exploredNodesSet + nextNodeFromStackKey

          val nextNodeFromStackGetVal: Option[NodeMapValFieldsStatic] =
            graph
            .get(nextNodeFromStackKey)
          /*side effect*/
          if (nextNodeFromStackGetVal.isDefined) {
            nextNodeFromStackGetVal
            .get
            .adjustedNodes
            .foreach((node: Int) => {nodesStackList = node +: nodesStackList})
            /*recursion*/
            innerLoop()
          } else /*if (nextNodeFromStackGetVal.isEmpty)*/ {
            /*'startNode' not in 'graph'*/
            /*return value*/
            BitSet.empty
          }
          /*recursion*/
          //innerLoop()
        }
        /*recursion*/
        //innerLoop()
      }
    }

    /*initialization*/
    innerLoop()
  }

  /*Depth-first search (DFS)*/
  /*must skip not existing starting 'node' values*/
  def nodesCounterDFS(
                       graph: Map[Int, NodeMapValFieldsStatic],
                       /*start node*/
                       startNode: Int,
                       explored:
                       BitSet =
                       BitSet.empty
                       ): Int = {
    /*state*/
    var exploredNodesSet:
    BitSet =
      explored
    //BitSet.empty
    /*accum*/
    var nodesStackList: List[Int] =
      List(startNode)
    var nodesCounter: Int = 0

    @scala.annotation.tailrec
    def innerLoop(
                   adjacentEdges: Set[Int] =
                   Set.empty
                   ): Int = {
      if (nodesStackList.isEmpty) {
        /*return value*/
        //exploredNodesSet
        nodesCounter
      } else /*if (nodesStackList.nonEmpty)*/ {
        val nextNodeFromStackKey: Int =
          nodesStackList.head

        /*side effect*/
        /*reduce loop condition to converge*/
        nodesStackList = nodesStackList.tail

        if (
        //nextNodeFromStack.isExplored
          exploredNodesSet
          .contains(nextNodeFromStackKey)
        ) {
          /*to recursion*/
          /*skip current 'node', check next*/
          /*recursion*/
          innerLoop()
        } else /*if (nextNodeFromStack.notExplored)*/ {
          /*side effect*/
          /*actual result*/
          exploredNodesSet =
            exploredNodesSet + nextNodeFromStackKey
          /*must be equal to exploredNodesSet.size*/
          nodesCounter += 1

          val nextNodeFromStackGetVal: Option[NodeMapValFieldsStatic] =
            graph
            .get(nextNodeFromStackKey)
          /*side effect*/
          if (nextNodeFromStackGetVal.isDefined) {
            nextNodeFromStackGetVal
            .get
            .adjustedNodes
            .foreach((node: Int) => {nodesStackList = node +: nodesStackList})
            /*recursion*/
            innerLoop()
          } else /*if (nextNodeFromStackGetVal.isEmpty)*/ {
            /*'startNode' not in 'graph'*/
            /*return value*/
            //BitSet.empty
            nodesCounter
          }
          /*recursion*/
          //innerLoop()
        }
        /*recursion*/
        //innerLoop()
      }
    }

    /*initialization*/
    innerLoop()
  }

  /*Depth-first search (DFS)*/
  /*must skip not existing starting 'node' values*/
  def pre_PostOrderDFS_ver3(
                             graph: Map[Int, NodeMapValFieldsStatic],
                             /*start node*/
                             startNodeKey: Int,
                             /*passed from 'DFS_Ordering'*/
                             explored:
                             BitSet =
                             BitSet.empty,
                             preOrd:
                             List[Int] =
                             List.empty,
                             /*new added when
                             ?reached `sink` / nowhere to go else?
                             'node'.isExplored &&
                             (adjusted.empty || adjusted.each.isExplored)
                             or when backtracks to itself ?
                             * */
                             postOrd:
                             List[Int] =
                             List.empty,
                             counted: Int = 0
                             //): BitSet = {
                             ): DFSResults = {
    /*assume(graph
           .get(startNodeKey).isDefined,"'key' must be within 'graph'")*/

    /*state*/
    var exploredNodesSet:
    BitSet =
      explored
    //BitSet.empty
    /*accum*/
    /*'nodes' added before corresponding adjusted list going to be checked*/
    var exploredNodesPre:
    List[Int] =
      preOrd
    //List.empty
    /*Stream[Int] =
    Stream.empty,*/
    /*'nodes' added after corresponding adjusted list was checked*/
    var exploredNodesPost:
    /*Stream[Int] =
    Stream.empty,*/
    List[Int] =
    //List.empty
      postOrd
    //List(startNode)
    /*all 'nodes' to check*/
    /*Last-in-First-out*/
    var nodesStackList: List[Int] =
    /*`push`*/
      List(startNodeKey)
    var nodesStackSet:
    BitSet =
      BitSet.empty
    /*first-In-first-Out*/
    /*var nodesQueue:
    scala.collection.immutable
    .Queue[StackLeader] =
    //.Queue[Int] =
    //Queue.empty
    //Queue(startNodeKey)
      Queue(
             StackLeader(
                          leaderKey = startNodeKey,
                          nodeKey = startNodeKey
                        ))*/
    //nodesQueue = nodesQueue.enqueue(startNodeKey)
    var nodesCounter: Int =
      counted
    //0

    /*
    to better handle possible `cases` (`flow`), may use:
    val pf: PartialFunction[Int, String] = {
      case i if i%2 == 0 => "even"
    }
    and they may be `composed` with 'orElse'
    val tf: (Int => String) = pf orElse { case _ => "odd"}
    * */
    @scala.annotation.tailrec
    def innerLoop(
                   /*adjacentEdges: Set[Int] =
                   Set.empty*/
                   //doneNode:
                   //Option[Int] = None
                   //doneNodeKey:                 Int
                   //lastCheckedNodeKey: Option[Int] = None
                   ): DFSResults = {
      //): BitSet = {
      if (
        nodesStackList.isEmpty
      //nodesQueue.isEmpty
      ) {
        /*all induced / incident / reachable 
        from 'startNode' 'nodes' was checked
        */
        /*side effect*/
        //exploredNodesPost =
        //exploredNodesPost :+ startNodeKey
        //startNode +: exploredNodesPost
        /*if (
          //doneNode
          lastCheckedNodeKey
          .isDefined) {
          if (
            exploredNodesSet
            .contains(
                startNodeKey
                     )
          ) {*/
        /*'append', then 'reverse' somewhere later*/
        //doneNode.get +:
        exploredNodesPost = exploredNodesPost :+
          startNodeKey
        //lastCheckedNodeKey.get
        /*} else {
          //startNodeGet.isEmpty
          //exploredNodesPost
        }
      } else {
        /*return value*/
        //exploredNodesPost
      }*/
        /*return value*/
        DFSResults(
                    /*same value for 'preOrder'*/
                    preOrder = exploredNodesPre,
                    postOrder = exploredNodesPost,
                    reachable = nodesCounter
                  )
        //exploredNodesSet
      } else /*if (nodesStackList.nonEmpty)*/ {
        /*last-In-first-Out*/
        val nextNodeKeyFromStack: Int =
        /*`topLookUp`*/
          nodesStackList.head

        /*side effect*/
        /*reduce loop condition to converge*/
        /*`pop`*/
        nodesStackList = nodesStackList.tail
        nodesStackSet = nodesStackSet - nextNodeKeyFromStack
        //val (firstTopElem, restOfQueue) =
        /*val (nextNodeKeyFromQueue, restOfQueue) =
          nodesQueue.dequeue
        /*side effect*/
        nodesQueue = restOfQueue*/

        //exploredNodesPost =
        /*if (
        //doneNode.isDefined
          nextNodeKeyFromQueue.leaderKey !=
            //startNodeKey
          doneNodeKey
        ) {*/
        /*if (
          exploredNodesSet
          .contains(
              startNode
                   )
        ) {*/
        /*'append', then 'reverse' somewhere later*/
        //doneNode.get +:
        /*`prepend`*/
        //exploredNodesPost :+ startNodeKey
        /*} else {
          //startNodeGet.isEmpty
          exploredNodesPost
        }*/
        /*} else {
          /*return value*/
          exploredNodesPost
        }*/

        if (
        //nextNodeFromStack.isExplored
          exploredNodesSet
          .contains(
              nextNodeKeyFromStack
              //nextNodeKeyFromQueue
              //nextNodeKeyFromQueue.nodeKey
                   )
        ) {
          /*if (
            //nodesStackSet.contains(nextNodeKeyFromStack)
            lastCheckedNodeKey.isEmpty
          ) {
            /*same*/
            //exploredNodesPost
          } else {
          exploredNodesPost =
            exploredNodesPost :+
              lastCheckedNodeKey.get
          /*nextNodeKeyFromStack +:
            exploredNodesPost*/
          }*/
          /*to recursion*/
          /*skip current 'node', check next*/
          /*recursion*/
          innerLoop()
          //innerLoop(doneNodeKey = doneNodeKey)
        } else /*if (nextNodeFromStack.notExplored)*/ {

          val nextNodeFromStackGetVal: Option[NodeMapValFieldsStatic] =
            graph
            .get(nextNodeKeyFromStack)
          /*val nextNodeFromQueueGetVal: Option[NodeMapValFieldsStatic] =
            graph
            .get(
                //nextNodeKeyFromQueue
                nextNodeKeyFromQueue.nodeKey
                )*/
          /*side effect*/
          if (
            nextNodeFromStackGetVal.isDefined
          //nextNodeFromQueueGetVal.isDefined
          ) {
            /*actual result*/
            exploredNodesSet =
              exploredNodesSet +
                nextNodeKeyFromStack
            /*nextNodeKeyFromQueue
            .nodeKey*/
            //var isNodeDone: Boolean = true

            nextNodeFromStackGetVal
            //nextNodeFromQueueGetVal
            .get
            .adjustedNodes
            .foreach((node: Int) => {
              if (nodesStackSet.contains(node)) {
              } else {
                /*`push`*/
                nodesStackList = node +: nodesStackList
                nodesStackSet = nodesStackSet + node
                //isNodeDone = false
              }
              /*nodesQueue =
                nodesQueue
                .enqueue(
                    //node
                    StackLeader(
                                 leaderKey = startNodeKey,
                                 nodeKey = node
                               )
                        )*/
            })
            /*side effect*/
            exploredNodesPre =
              /*'append'*/
              exploredNodesPre :+
                /*nextNodeKeyFromQueue
                .nodeKey*/
                /*`prepend`*/
                nextNodeKeyFromStack //+: exploredNodesPre
            if (
            //isNodeDone
              true
            ) {
              exploredNodesPost =
                exploredNodesPost :+
                  nextNodeKeyFromStack /*+:
            exploredNodesPost*/
            } else {
              /*same*/
              //exploredNodesPost
            }
            nodesCounter += 1
            //nodesCounter + 1
            /*recursion*/
            innerLoop()
            //innerLoop(doneNodeKey = nextNodeKeyFromQueue.nodeKey)
            /*'startNodeKey' was in stack too*/
            //innerLoop(lastCheckedNodeKey = Some(nextNodeKeyFromStack))
          } else /*if (nextNodeFromStackGetVal.isEmpty)*/ {
            /*?may be check it outside the 'innerLoop()'?*/
            /*'startNode' not in 'graph'*/
            /*return value*/
            //BitSet.empty
            DFSResults(
                        /*same value for 'preOrder'*/
                        preOrder = exploredNodesPre,
                        postOrder = exploredNodesPost,
                        reachable = nodesCounter
                      )
          }
          /*recursion*/
          //innerLoop()
        }
        /*recursion*/
        //innerLoop()
      }
    }

    /*initialization*/
    innerLoop()
    //innerLoop(doneNodeKey = startNodeKey)
  }

  /*Depth-first search (DFS)*/
  /*must skip not existing starting 'node' values*/
  @scala.annotation.tailrec
  def pre_PostOrderDFS_ver4(
                             /*immutable, passed around*/
                             graph: Map[Int, NodeMapValFieldsStatic],
                             /*start node*/
                             startNodeKey: Int,
                             /*passed from 'DFS_Ordering'*/
                             explored:
                             BitSet =
                             BitSet.empty,
                             /*TODO review redundant parameters*/
                             nodesToCheckSet:
                             BitSet =
                             BitSet.empty,
                             nodesRemainsToCheck: List[Int] =
                             List.empty,
                             dfsResults: DFSResults =
                             DFSResults(
                                         preOrder =
                                           List.empty,
                                         /*new added when 'node.done'
                                         ?reached `sink` / nowhere to go else?
                                         'node'.isExplored &&
                                         (adjusted.empty || adjusted.each
                                         .isExplored)
                                         or when backtracks to itself ?
                                         * */
                                         postOrder =
                                           List.empty,
                                         reachable = 0
                                       )
                             ): DFSResults = {
    /*assume(graph
           .get(startNodeKey).isDefined,"'key' must be within 'graph'")*/
    if (
      explored.contains(startNodeKey) &&
        nodesRemainsToCheck.isEmpty
    ) {
      /*return value*/
      dfsResults
    } else if (
             explored.contains(startNodeKey) &&
               nodesRemainsToCheck.nonEmpty
           ) {
      //assume(!nodesToCheckSet.contains(startNodeKey))

      val nodesAdjusted: Set[Int] =
        graph
        .get(startNodeKey)
        .get
        .adjustedNodes
      if (nodesAdjusted.isEmpty) {
        /*nowhere to go, `sink`*/
        /*`startNodeKey` done*/
        /*return value*/
        DFSResults(
                    dfsResults.preOrder,
                    /*`append`*/
                    dfsResults.postOrder :+ startNodeKey,
                    dfsResults.reachable
                  )
      } else /*if (nodesAdjusted.nonEmpty)*/ {
        /*TODO two similar checks, at least one redundant*/
        if (nodesAdjusted.subsetOf(explored)) {
          /*all nodes within already explored*/
          /*`startNodeKey` done*/
          /*return value*/
          DFSResults(
                      dfsResults.preOrder,
                      /*`append`*/
                      dfsResults.postOrder :+ startNodeKey,
                      dfsResults.reachable
                    )
        } else /*remains unexplored*/ {
          /*possibly time consuming operations*/
          val adjustedUnexplored: Set[Int] =
            nodesAdjusted
            .diff(explored)
          if (adjustedUnexplored.isEmpty) {
            /*`startNodeKey` done*/
            /*return value*/
            DFSResults(
                        dfsResults.preOrder,
                        /*`append`*/
                        dfsResults.postOrder :+ startNodeKey,
                        dfsResults.reachable
                      )
          } else /*if (adjustedUnexplored.nonEmpty)*/ {
            val nextNodeKeyToCheck:
            //Option[Int] =
            Int =
              adjustedUnexplored
              .head
            //.headOption
            val exploredUpdated: BitSet =
              explored + nextNodeKeyToCheck
            /*val nodesAdjustedUpdated: Set[Int] =
          nodesAdjusted - nextNodeKeyToCheck*/
            /*recursion*/
            pre_PostOrderDFS_ver4(
                                   graph = graph,
                                   startNodeKey: Int,
                                   /*passed from 'DFS_Ordering'*/
                                   explored =
                                     exploredUpdated,
                                   nodesToCheckSet = nodesToCheckSet,
                                   nodesRemainsToCheck = nodesRemainsToCheck,
                                   dfsResults = DFSResults(
                                                            /*`append`*/
                                                            dfsResults
                                                            .preOrder :+
                                                              startNodeKey,
                                                            dfsResults
                                                            .postOrder,
                                                            dfsResults
                                                            .reachable + 1
                                                          )
                                 )
          }
        }
      }
    } else /*if (!explored.contains(startNodeKey))*/ {
      /*fist iteration, `fresh` node*/
      val exploredUpdated: BitSet =
        explored + startNodeKey
      /*return value*/
      /*recursion*/
      pre_PostOrderDFS_ver4(
                             graph = graph,
                             startNodeKey: Int,
                             /*passed from 'DFS_Ordering'*/
                             explored =
                               exploredUpdated,
                             nodesToCheckSet = nodesToCheckSet,
                             nodesRemainsToCheck = nodesRemainsToCheck,
                             dfsResults = DFSResults(
                                                      /*`append`*/
                                                      dfsResults
                                                      .preOrder :+ startNodeKey,
                                                      dfsResults.postOrder,
                                                      dfsResults
                                                      .reachable + 1
                                                    )
                           )
    }

  }

  /*Reverse the directions of all `arcs` to obtain the `transpose` `graph`.*/
  @scala.annotation.tailrec
  def DFS_Ordering(
                    /*for lookUp*/
                    graph: Map[Int, NodeMapValFieldsStatic],
                    resultAccum: DFSResults =
                    DFSResults(List.empty, List.empty, 0),
                    //graphLength: Int,
                    //indexCounter: Int = 0,
                    mapKeyIter: Iterator[Int],
                    exploredNodesSet:
                    BitSet =
                    BitSet.empty
                    ): DFSResults = {
    if (mapKeyIter.isEmpty) {
      /*return value*/
      resultAccum
    } else {
      val currentNodeKey: Int =
        mapKeyIter
        /*implied / suggested that 'key' exist in map*/
        .next()
      //val currentNode: Option[NodeMapValFieldsStatic] =
      /*val currentNode: NodeMapValFieldsStatic =
        graph
        .get(currentNodeKey            )
      .get*/
      /*cases:
      * either new 'nodes' added or not (same `explored` list)*/
      /*?no 'concat' / 'union'?*/
      val DFSResults(
      resultAccumPreOrderUpdated,
      resultAccumPostOrderUpdated,
      totalNodes
                    ) =
        if (
        //!currentNode.node.isExplored
          !exploredNodesSet
           .contains(currentNodeKey)
        ) {
          pre_PostOrderDFS_ver3(
                                 graph = graph,
                                 startNodeKey = currentNodeKey,
                                 explored = exploredNodesSet,
                                 preOrd =
                                   resultAccum.preOrder,
                                 postOrd =
                                   resultAccum.postOrder,
                                 counted = resultAccum.reachable
                               )
        } else {
          /*return value*/
          DFSResults(
                      resultAccum.preOrder,
                      resultAccum.postOrder,
                      resultAccum.reachable
                    )
        }
      /*recursion*/
      DFS_Ordering(
                    graph = graph,
                    resultAccum =
                      DFSResults(
                                  resultAccumPreOrderUpdated,
                                  resultAccumPostOrderUpdated,
                                  totalNodes
                                ),
                    mapKeyIter = mapKeyIter,
                    exploredNodesSet = exploredNodesSet
                  )
    }
  }

  /*Reverse the directions of all `arcs` to obtain the `transpose` `graph`.*/
  @scala.annotation.tailrec
  def DFS_SCCs_Size(
                     /*for lookUp*/
                     graph: Map[Int, NodeMapValFieldsStatic],
                     resultAccum: Stream[Int] =
                     Stream.empty,
                     /*must be post order of a `transpose` `graph`*/
                     mapKeyIter: Iterator[Int],
                     exploredNodesSet:
                     BitSet =
                     BitSet.empty
                     ): Stream[Int] = {
    if (mapKeyIter.isEmpty) {
      /*return value*/
      resultAccum
    } else {
      val currentNodeKey: Int =
        mapKeyIter
        /*implied / suggested that 'key' exist in map*/
        .next()
      /*cases:
      * either new 'nodes' added or not (same `explored` list)*/
      /*?no 'concat' / 'union'?*/
      val resultsUpdated: Stream[Int] =
        if (
        //!currentNode.node.isExplored
          !exploredNodesSet
           .contains(currentNodeKey)
        ) {
          /*prepend*/
          nodesCounterDFS(
                           graph = graph,
                           startNode = currentNodeKey,
                           explored = exploredNodesSet
                         ) +: resultAccum
        } else {
          /*return value*/
          resultAccum
        }
      /*recursion*/
      DFS_SCCs_Size(
                     graph = graph,
                     resultAccum =
                       resultsUpdated,
                     mapKeyIter = mapKeyIter,
                     exploredNodesSet = exploredNodesSet
                   )
    }
  }

  /*Reverse the directions of all `arcs` to obtain the `transpose` `graph`.*/
  @scala.annotation.tailrec
  def DepthFirstOrder(
                       /*for lookUp*/
                       graph: Vector[ExplorableNodeWithAdjusted],
                       resultAccum: DepthFirstSearchResult =
                       //DepthFirstSearchResult(List.empty, List.empty),
                       DepthFirstSearchResult(Stream.empty, Stream.empty),
                       graphLength: Int,
                       indexCounter: Int = 0,
                       nodesValuesZeroBased: Boolean = false
                       ): DepthFirstSearchResult = {
    if (indexCounter >= graphLength) {
      /*return value*/
      resultAccum
    } else {
      val currentNode =
        graph(indexCounter)
      /*cases:
      * either new 'nodes' added or not (same `explored` list)*/
      /*?no 'concat' / 'union'?*/
      val DepthFirstSearchResult(
      //addedToResultAccumPreOrder,
      //addedToResultAccumPostOrder
      resultAccumPreOrderUpdated,
      resultAccumPostOrderUpdated
                                ) =
        if (!currentNode.node.isExplored) {
          preAndPostOrderDFS(
                              graph = graph,
                              v = currentNode.node.node,
                              exploredNodesPre =
                                resultAccum.preOrder,
                              exploredNodesPost =
                                resultAccum.postOrder //,
                              //nodesValuesZeroBased = nodesValuesZeroBased
                            )
        } else {
          DepthFirstSearchResult(
                                  resultAccum.preOrder,
                                  resultAccum.postOrder)
        }
      /*recursion*/
      DepthFirstOrder(
                       /*for lookUp*/
                       graph: Vector[ExplorableNodeWithAdjusted],
                       resultAccum =
                         DepthFirstSearchResult(
                                                 resultAccumPreOrderUpdated,
                                                 resultAccumPostOrderUpdated),
                       graphLength = graphLength,
                       indexCounter = indexCounter + 1
                     )
    }
    /*for {
      node <- graph
    } yield if (!node.node.isExplored) {
      preAndPostOrderDFS(
                          graph = graph,
                          v = node.node.node
                        )
    } else {
      DepthFirstSearchResult(List.empty, List.empty)
    }*/
  }

  /*Reverse the directions of all `arcs` to obtain the `transpose` `graph`.*/
  @scala.annotation.tailrec
  def DepthFirstPostOrder(
                           /*for lookUp*/
                           graph: Vector[ExplorableNodeWithAdjusted],
                           /*resultAccum: List[IsExploredNode] =
                           List.empty,*/
                           resultAccum: Stream[IsExploredNode] =
                           Stream.empty,
                           graphLength: Int,
                           indexCounter: Int = 0,
                           nodeIndexShift: Int = -1
                           ): Stream[IsExploredNode] = {
    //): List[IsExploredNode] = {
    if (indexCounter >= graphLength) {
      /*return value*/
      resultAccum
    } else {
      val currentNode: ExplorableNodeWithAdjusted =
        graph(indexCounter)
      /*cases:
      * either new 'nodes' added or not (same `explored` list)*/
      /*?no 'concat' / 'union'?*/
      //val resultAccumPostOrderUpdated: List[IsExploredNode] =
      def resultAccumPostOrderUpdated: Stream[IsExploredNode] =
        if (!currentNode.node.isExplored) {
          /*new*/
          postOrderDFS_Nodes(
                              graph = graph,
                              v = currentNode.node.node,
                              exploredNodes =
                                resultAccum //,
                              //nodeIndexShift = nodeIndexShift
                            )
        } else {
          /*same*/
          resultAccum
        }
      /*recursion*/
      DepthFirstPostOrder(
                           /*for lookUp*/
                           graph: Vector[ExplorableNodeWithAdjusted],
                           resultAccum =
                             resultAccumPostOrderUpdated,
                           graphLength = graphLength,
                           indexCounter = indexCounter + 1
                         )
    }
  }

  /*check `nodes` in Reverse `postOrder` (needed) of
   the `transpose` `graph` and find & extract / create all SCCs*/
  @scala.annotation.tailrec
  def transposeDepthFirstOrderSCCs(
                                    /*for lookUp*/
                                    /*must be reset as `unExplored`*/
                                    graph: Vector[ExplorableNodeWithAdjusted],
                                    /*check order and termination condition*/
                                    /*must be reset as `unExplored`*/
                                    /*preOrderRemains: List[IsExploredNode],
                                    resultAccum: List[List[IsExploredNode]] =
                                    List.empty,*/
                                    preOrderRemains: => Stream[IsExploredNode],
                                    resultAccum: =>
                                    Stream[Stream[IsExploredNode]] =
                                    Stream.empty,
                                    graphLength: Int,
                                    indexCounter: Int = 0,
                                    nodesValuesZeroBased: Boolean = false,
                                    minNodeVal: Int = 1,
                                    nodeIndexShift: Int = -1
                                    ): Stream[Stream[IsExploredNode]] = {
    //): List[List[IsExploredNode]] = {
    if (
    //indexCounter >= graphLength
      preOrderRemains.isEmpty
    ) {
      /*return value*/
      resultAccum
    } else {
      val currentNode: ExplorableNodeWithAdjusted =
      //graph(indexCounter)
        graph(preOrderRemains.head.node + nodeIndexShift)
      /*cases:
      * either new 'nodes' added or not (same `explored` list)*/
      /*?no 'concat' / 'union'?*/
      //val newSCC: List[IsExploredNode] =
      //val updatedSCCsResult: List[List[IsExploredNode]] =
      def updatedSCCsResult: Stream[Stream[IsExploredNode]] =
        if (!currentNode.node.isExplored) {
          /*? order does not matter ?*/
          /*prePend*/
          preOrderDFS(
                       graph = graph,
                       v = currentNode.node.node
                     ) +: resultAccum
        } else {

          resultAccum
        }
      /*recursion*/
      transposeDepthFirstOrderSCCs(
                                    /*for lookUp*/
                                    graph: Vector[ExplorableNodeWithAdjusted],
                                    preOrderRemains =
                                      preOrderRemains.tail,
                                    resultAccum =
                                      updatedSCCsResult,
                                    graphLength = graphLength,
                                    indexCounter = indexCounter + 1,
                                    nodeIndexShift = nodeIndexShift
                                  )
    }
  }

  /*check `nodes` in Reverse `postOrder` (needed) of
   the `transpose` `graph` and find all SCCs sizes*/
  @scala.annotation.tailrec
  def transposeDepthFirstOrderSCCsSize(
                                        /*for lookUp*/
                                        /*must be reset as `unExplored`*/
                                        graph: =>
                                        Vector[ExplorableNodeWithAdjusted],
                                        /*check order and termination
                                        condition*/
                                        /*must be reset as `unExplored`*/
                                        postOrderRemains: =>
                                        List[IsExploredNode],
                                        /*Pass streams around via
                                        `by-name` parameters*/
                                        /*postOrderRemains: =>
                                        Stream[IsExploredNode],*/
                                        /*resultAccum: List[Int] =
                                        List.empty,*/
                                        resultAccum: => Stream[Int] =
                                        Stream.empty,
                                        //graphLength: Int,
                                        //indexCounter: Int = 0,
                                        //nodesValuesZeroBased: Boolean = false,
                                        minNodeVal: Int = 1,
                                        nodeIndexShift: Int = -1
                                        ): Stream[Int] = {
    if (
    //indexCounter >= graphLength
      postOrderRemains.isEmpty
    ) {
      /*return value*/
      resultAccum
    } else /*if (
      postOrderRemains.nonEmpty
    )*/ {
      val currentNode: ExplorableNodeWithAdjusted =
      //graph(indexCounter)
        graph(postOrderRemains.head.node + nodeIndexShift)
      /*cases:
      * either new 'nodes' added or not (same `explored` list)*/
      /*?no 'concat' / 'union'?*/
      //val newSCC: List[IsExploredNode] =
      def updatedSCCsResult: Stream[Int] =
      //val updatedSCCsResult: Stream[Int] =
        if (!currentNode.node.isExplored) {
          /*new*/
          /*? order does not matter ?*/
          /*prePend*/
          /*only 'nodes' amount in SCC needed / interested */
          maxPostOrderDFS(
                           graph = graph,
                           v = currentNode.node.node
                         ) +: resultAccum
        } else {
          /*same*/
          resultAccum
        }
      /*recursion*/
      transposeDepthFirstOrderSCCsSize(
                                        /*for lookUp*/
                                        graph:
                                          Vector[ExplorableNodeWithAdjusted],
                                        postOrderRemains =
                                          postOrderRemains.tail,
                                        resultAccum =
                                          updatedSCCsResult,
                                        //graphLength = graphLength,
                                        //indexCounter = indexCounter + 1,
                                        nodeIndexShift = nodeIndexShift
                                      )
    }
  }

  //input: graph G = (V, E)
  //output: set of strongly connected components (sets of vertices)
  def tarjan(
              adjacencyList: Vector[IndexedNodeWithAdjacencyList],
              index: Int = 0,
              stack: List[IndexedNode] =
              List.empty[IndexedNode]
              ): Vector[List[Int]] = {
    //): List[List[Int]] = {
    /*global 'index' tracker*/
    //var index: Int = 0
    //S := empty
    /*
    Use 'List' instead:
    'stack.push' 'x' becomes
    'x :: list'
    'stack.pop' is 'list.tail'
     */
    //val stack /*: Stack[Nothing]*/ =
    //scala.collection.immutable.Stack.empty[IndexedNode]
    //List.empty[IndexedNode]

    /*auxiliary method*/
    def createNewSCC(
                      newSCC: List[Int] =
                      List.empty,
                      nodeFromStack: Option[IndexedNode] =
                      None,
                      /*assume that 'v.inStack'*/
                      currentStack: /*Stack*/ List[IndexedNode],
                      v: IndexedNode
                      ): List[Int] = {
      if (
        nodeFromStack.isDefined &&
          nodeFromStack.get == v
      ) {
        /*return value*/
        newSCC.reverse
      } else {
        /*val (w, stackWithoutTop): (IndexedNode, Stack[IndexedNode]) =
          stack.pop*/
        val stackWithoutTop = currentStack.tail
        val w = currentStack.head
        /*side effect*/
        w.isInStack = false
        /*recursion*/
        createNewSCC(
                      newSCC =
                        w.nodeVal +: newSCC,
                      nodeFromStack = Some(w),
                      currentStack = stackWithoutTop,
                      v = v
                    )
      }
    }

    /*recursive method, what is the output, except side effects ?*/
    /*changes 'index', so must return new value*/
    /*create discovered 'SCC', so must return it*/
    /*changes 'stack', so must return it*/
    def strongConnect(
                       v: IndexedNode,
                       index: Int
                       ): SCC_Result = {
      //): List[Int] = {
      /*auxiliary method*/
      /*trying to find `min` val*/
      def minLowLink(
                      //nodeAndAdjusted: IndexedNodeWithAdjacencyList,
                      adjustedNodes: List[IndexedNode],
                      /*actually started with 'v.nodeLowLink'*/
                      currentMin: Int = Int.MaxValue,
                      v: IndexedNode,
                      /*global index state changing with time*/
                      index: Int
                      ): Int = {
        if (adjustedNodes.isEmpty) {
          /*return value*/
          currentMin
        } else {
          val adjNode: IndexedNode = adjustedNodes.head

          /*? partial function ?*/
          //val newMin: Int =
          if (adjNode.nodeIndex.isEmpty) {
            // Successor 'w'
            // has not yet been visited recurse on it
            /*outer recursion*/
            /*?must change 'v.nodeLowLink'?*/
            val SCC_Result(newLowlink, newIndex, newStack, newSCC) =
              strongConnect(
                             adjNode,
                             /*?must return as global state?*/
                             //newIndex
                             index
                           )
            /*side effect*/
            v.nodeLowLink = newLowlink
            /*recursion*/
            minLowLink(
                        adjustedNodes =
                          /*must converge eventually*/
                          adjustedNodes.tail,
                        currentMin =
                          newLowlink.min(adjNode.nodeLowLink),
                        //v.nodeLowLink.min(adjNode.nodeLowLink),
                        v: IndexedNode,
                        newIndex
                      )
            /*return value*/
            //v.nodeLowLink.min(adjNode.nodeLowLink)
          } else if (adjNode.isInStack) {
            // Successor 'w' is in `stack` S and
            // hence in the current `SCC`
            /*recursion*/
            minLowLink(
                        adjustedNodes =
                          /*must converge eventually*/
                          adjustedNodes.tail,
                        currentMin =
                          v.nodeLowLink
                          .min(adjNode.nodeIndex.getOrElse(v.nodeLowLink)),
                        v: IndexedNode,
                        index
                      )
            /*return value*/
            //v.nodeLowLink.min(adjNode.nodeIndex.getOrElse(v.nodeLowLink))
          } else {
            /*return value*/
            //currentMin
            /*recursion*/
            minLowLink(
                        adjustedNodes =
                          /*must converge eventually*/
                          adjustedNodes.tail,
                        currentMin =
                          currentMin,
                        v: IndexedNode,
                        index
                      )
          }
          /*recursion*/
          /*minLowLink(
                      adjustedNodes=
                        /*must converge eventually*/
                        adjustedNodes.tail,
                      currentMin=
                        newMin,
                      v: IndexedNode,
                      index
                    )*/
        }
      }

      // Set the depth index for 'v' to the smallest unused `index`
      /*initialization*/
      /*side effects*/
      v.nodeIndex = Some(index)
      /*?And what is initial value +infinity?*/
      v.nodeLowLink = index
      /*change state*/
      //index = index + 1
      val newIndex = index + 1
      //stack.push(v)
      val vInStack = v +: stack
      /*side effect*/
      v.isInStack = true

      /*must be separate method*/
      // Consider `successors` of 'v'
      //for each(v, w) in E do
      val nodeAndAdjusted: IndexedNodeWithAdjacencyList =
        adjacencyList(v.nodeVal - 1)
      /*trying to find `min` val*/
      v.nodeLowLink =
        minLowLink(
                    adjustedNodes =
                      nodeAndAdjusted.adjustedNodes,
                    currentMin =
                      v.nodeLowLink,
                    v: IndexedNode,
                    /*? may change within ?*/
                    index = newIndex
                  )
      //(
      /*for {
        adjNode <- nodeAndAdjusted.adjustedNodes
      } yield
      /*? partial function ?*/
      //if (w.index is undefined) then
        if (adjNode.nodeIndex.isEmpty) {
          // Successor 'w'
          // has not yet been visited recurse on it
          /*recursion*/
          strongConnect(
                         adjNode,
                        /*?must return as global state?*/
                        newIndex
                       )
          /*side effect*/
          v.nodeLowLink =
            v.nodeLowLink.min(adjNode.nodeLowLink)
          /*return*/
          v.nodeLowLink
        } else if (adjNode.inStack) {
          // Successor 'w' is in `stack` S and
          // hence in the current `SCC`
          /*side effect*/
          v.nodeLowLink =
            v.nodeLowLink
            .min(adjNode.nodeIndex.getOrElse(v.nodeLowLink))
          /*return*/
          v.nodeLowLink
        }*/
      //).head

      // If 'v' is a `root` node,
      // 'pop' the `stack` and
      // generate an `SCC`
      /*assume that 'v.nodeIndex.isDefined'*/
      if (v.nodeLowLink == v.nodeIndex.get) {
        //start a new strongly connected component
        //val newSCC: List[Int] = List.empty
        //repeat
        /*val (w, stackWithoutTop): (IndexedNode, Stack[IndexedNode]) =
          stack.pop
        w.inStack = false*/
        //add 'w' to `current` `strongly connected component`
        //w +: newSCC
        //until(w = v)
        //output the current strongly connected component

        /*return value*/
        //newSCC.reverse
        SCC_Result(
                    v.nodeLowLink,
                    newIndex,
                    vInStack,
                    createNewSCC(
                                  newSCC =
                                    List.empty,
                                  nodeFromStack = None,
                                  currentStack = vInStack,
                                  v = v
                                )
                  )
      } else {
        /*return value*/
        SCC_Result(
                    v.nodeLowLink,
                    newIndex,
                    vInStack,
                    List.empty[Int]
                  )
      }
    }

    /*instead must call recursion on itself*/
    /*return value*/
    for {
      v <- adjacencyList //G.V
      /*guard*/
      //if (v.index is undefined)
      if v.node.nodeIndex.isEmpty
    } yield strongConnect(
                           v.node,
                           /*incremented every iteration within method call*/
                           index
                         ).sCC
  }

  //input: graph G = (V, E)
  //output: set of strongly connected components (sets of vertices)
  def tarjanWithMap(
                     adjacencyList: Map[Int, NodeMapValFieldsStatic],
                     index: Int = 0,
                     stack: List[IndexedNode] =
                     List.empty[IndexedNode]
                     ): Vector[List[Int]] = {
    //): List[List[Int]] = {
    /*global 'index' tracker*/
    //var index: Int = 0
    //S := empty
    /*
    Use 'List' instead:
    'stack.push' 'x' becomes
    'x :: list'
    'stack.pop' is 'list.tail'
     */
    //val stack /*: Stack[Nothing]*/ =
    //scala.collection.immutable.Stack.empty[IndexedNode]
    //List.empty[IndexedNode]

    /*auxiliary method*/
    def createNewSCC(
                      newSCC: List[Int] =
                      List.empty,
                      nodeFromStack: Option[IndexedNode] =
                      None,
                      /*assume that 'node.inStack'*/
                      currentStack: /*Stack*/ List[IndexedNode],
                      v: IndexedNode
                      ): List[Int] = {
      if (
        nodeFromStack.isDefined &&
          nodeFromStack.get == v
      ) {
        /*return value*/
        newSCC.reverse
      } else {
        /*val (w, stackWithoutTop): (IndexedNode, Stack[IndexedNode]) =
          stack.pop*/
        val stackWithoutTop = currentStack.tail
        val w = currentStack.head
        /*side effect*/
        w.isInStack = false
        /*recursion*/
        createNewSCC(
                      newSCC =
                        w.nodeVal +: newSCC,
                      nodeFromStack = Some(w),
                      currentStack = stackWithoutTop,
                      v = v
                    )
      }
    }

    /*recursive method, what is the output, except side effects ?*/
    /*changes 'index', so must return new value*/
    /*create discovered 'SCC', so must return it*/
    /*changes 'stack', so must return it*/
    def strongConnect(
                       /*?just 'key'?*/
                       node:
                       Int,
                       //IndexedNode,
                       index: Int
                       ): SCC_Result = {
      //): List[Int] = {
      /*auxiliary method*/
      /*trying to find `min` val*/
      def minLowLink(
                      //nodeAndAdjusted: IndexedNodeWithAdjacencyList,
                      adjustedNodes:
                      Set[Int],
                      //List[IndexedNode],
                      /*actually started with 'node.nodeLowLink'*/
                      currentMin: Int = Int.MaxValue,
                      node: IndexedNode,
                      /*global index state changing with time*/
                      index: Int
                      ): Int = {
        if (adjustedNodes.isEmpty) {
          /*return value*/
          currentMin
        } else {
          val adjNode: IndexedNode =
          //adjustedNodes.head
            IndexedNode(
                         nodeVal = adjustedNodes.head,
                         nodeIndex = None,
                         nodeLowLink = adjustedNodes.head,
                         isInStack = false
                       )

          /*? partial function ?*/
          //val newMin: Int =
          if (adjNode.nodeIndex.isEmpty) {
            // Successor 'w'
            // has not yet been visited recurse on it
            /*outer recursion*/
            /*?must change 'node.nodeLowLink'?*/
            val SCC_Result(newLowlink, newIndex, newStack, newSCC) =
              strongConnect(
                             //adjNode,
                             adjNode.nodeVal,
                             /*?must return as global state?*/
                             //newIndex
                             index
                           )
            /*side effect*/
            node.nodeLowLink = newLowlink
            /*recursion*/
            minLowLink(
                        adjustedNodes =
                          /*must converge eventually*/
                          adjustedNodes.tail,
                        currentMin =
                          newLowlink.min(adjNode.nodeLowLink),
                        //node.nodeLowLink.min(adjNode.nodeLowLink),
                        node: IndexedNode,
                        newIndex
                      )
            /*return value*/
            //node.nodeLowLink.min(adjNode.nodeLowLink)
          } else if (adjNode.isInStack) {
            // Successor 'w' is in `stack` S and
            // hence in the current `SCC`
            /*recursion*/
            minLowLink(
                        adjustedNodes =
                          /*must converge eventually*/
                          adjustedNodes.tail,
                        currentMin =
                          node.nodeLowLink
                          .min(adjNode.nodeIndex.getOrElse(node.nodeLowLink)),
                        node: IndexedNode,
                        index
                      )
            /*return value*/
            //node.nodeLowLink.min(adjNode.nodeIndex.getOrElse(node
            // .nodeLowLink))
          } else {
            /*return value*/
            //currentMin
            /*recursion*/
            minLowLink(
                        adjustedNodes =
                          /*must converge eventually*/
                          adjustedNodes.tail,
                        currentMin =
                          currentMin,
                        node: IndexedNode,
                        index
                      )
          }
          /*recursion*/
          /*minLowLink(
                      adjustedNodes=
                        /*must converge eventually*/
                        adjustedNodes.tail,
                      currentMin=
                        newMin,
                      node: IndexedNode,
                      index
                    )*/
        }
      }

      // Set the depth index for 'node' to the smallest unused `index`
      /*initialization*/
      /*side effects*/
      //node.nodeIndex = Some(index)
      /*?And what is initial value +infinity?*/
      //node.nodeLowLink = index
      /*change state*/
      //index = index + 1
      val newIndex = index + 1
      //stack.push(node)
      val vInStack: List[IndexedNode] =
      //node +:
        IndexedNode(
                     nodeVal = node,
                     nodeIndex = None,
                     nodeLowLink = node,
                     isInStack = false
                   ) +:
          stack
      /*side effect*/
      //node.inStack = true

      /*must be separate method*/
      // Consider `successors` of 'node'
      //for each(node, w) in E do
      //val nodeAndAdjusted: IndexedNodeWithAdjacencyList =
      //adjacencyList(node.nodeVal - 1)
      val adjustedNodesKeys: Set[Int] =
        adjacencyList.get(
                           node
                           //.nodeVal
                         ).get.adjustedNodes
      /*trying to find `min` val*/
      //node.nodeLowLink =
      minLowLink(
                  adjustedNodes =
                    //nodeAndAdjusted.adjustedNodes,
                    adjustedNodesKeys,
                  currentMin =
                    node,
                  //node.nodeLowLink,
                  node =
                    IndexedNode(
                                 nodeVal = node,
                                 nodeIndex = None,
                                 nodeLowLink = node,
                                 isInStack = false
                               ),
                  /*? may change within ?*/
                  index = newIndex
                )
      //(
      /*for {
        adjNode <- nodeAndAdjusted.adjustedNodes
      } yield
      /*? partial function ?*/
      //if (w.index is undefined) then
        if (adjNode.nodeIndex.isEmpty) {
          // Successor 'w'
          // has not yet been visited recurse on it
          /*recursion*/
          strongConnect(
                         adjNode,
                        /*?must return as global state?*/
                        newIndex
                       )
          /*side effect*/
          node.nodeLowLink =
            node.nodeLowLink.min(adjNode.nodeLowLink)
          /*return*/
          node.nodeLowLink
        } else if (adjNode.inStack) {
          // Successor 'w' is in `stack` S and
          // hence in the current `SCC`
          /*side effect*/
          node.nodeLowLink =
            node.nodeLowLink
            .min(adjNode.nodeIndex.getOrElse(node.nodeLowLink))
          /*return*/
          node.nodeLowLink
        }*/
      //).head

      // If 'node' is a `root` node,
      // 'pop' the `stack` and
      // generate an `SCC`
      /*assume that 'node.nodeIndex.isDefined'*/
      if (
      //node.nodeLowLink == node.nodeIndex.get
        true
      ) {
        //start a new strongly connected component
        //val newSCC: List[Int] = List.empty
        //repeat
        /*val (w, stackWithoutTop): (IndexedNode, Stack[IndexedNode]) =
          stack.pop
        w.inStack = false*/
        //add 'w' to `current` `strongly connected component`
        //w +: newSCC
        //until(w = node)
        //output the current strongly connected component

        /*return value*/
        //newSCC.reverse
        SCC_Result(
                    //node.nodeLowLink,
                    node,
                    newIndex,
                    vInStack,
                    createNewSCC(
                                  newSCC =
                                    List.empty,
                                  nodeFromStack = None,
                                  currentStack = vInStack,
                                  v =
                                    //node
                                    IndexedNode(
                                                 nodeVal = node,
                                                 nodeIndex = None,
                                                 nodeLowLink = node,
                                                 isInStack = false
                                               )
                                )
                  )
      } else {
        /*return value*/
        SCC_Result(
                    //node.nodeLowLink,
                    node,
                    newIndex,
                    vInStack,
                    List.empty[Int]
                  )
      }
    }

    /*instead ?must? call recursion on itself*/
    /*return value*/
    (for {
    //adjustedNode <- adjacencyList.valuesIterator //G.V
      adjustedNode <- adjacencyList.keysIterator
    /*guard*/
    //if (node.index is undefined)
    //if node.node.nodeIndex.isEmpty
    } yield strongConnect(
                           //v.node,
                           adjustedNode,
                           /*incremented every iteration within method call*/
                           index
                         ).sCC).toVector
  }

  /*
  Done: replace 'Set' with 'List' or 'Stream' & check the effect
  - 'Stream' take a little less space then 'Set' according to the test
  if fails,
  replace 'Map' with 'Array',
  as it's size known ahead ('nodes' are just a range from '1' to '875714') &
  'Array' is a native Java object
   */
  //input: graph G = (V, E)
  //output: set of strongly connected components (sets of vertices)
  def tarjanForDiGraphDyn(
                           adjacencyList:
                           //ArrayBuffer[NodeFieldsArray]
                           //Map[Int, Set[Int]]
                           Map[Int, NodeMapValFieldsDynamic] //,
                           /*index: Int = 0,
                           stack:
                           List[Int] =*/
                           //List[IndexedNode] =
                           //List[Map[Int, NodeMapValFieldsDynamic]] =
                           //List.empty
                           ): Stream[Int] = {
    //): List[Int] = {
    //): List[List[Int]] = {
    //): Iterable[List[Int]] = {
    //): Vector[List[Int]] = {
    var localIndex: Int = 0 //index
    var nodesOnStack: BitSet =
      BitSet.empty
    var graphSCCs:
    //List[List[Int]] =
    /*List[Int] =
      List.empty*/
    Stream[Int] =
      Stream.empty
    var localStack:
    List[Int] =
    //List[IndexedNode] =
    //stack
      List.empty
    //index := 0
    //S := empty

    /*function strongconnect(v)
    // Set the depth index for v to the smallest unused index
    v.index := index
    v.lowlink := index
    index := index + 1
    S.push(v)
    v.onStack := true

    // Consider successors of v
    for each (v, w) in E do
      if (w.index is undefined) then
    // Successor w has not yet been visited; recurse on it
    strongconnect(w)
    v.lowlink  := min(v.lowlink, w.lowlink)
    else if (w.onStack) then
    // Successor w is in stack S and hence in the current SCC
    v.lowlink  := min(v.lowlink, w.index)
    end if
          end for

    // If v is a root node, pop the stack and generate an SCC
    if (v.lowlink = v.index) then
    start a new strongly connected component
    repeat
    w := S.pop()
    w.onStack := false
    add w to current strongly connected component
    until (w = v)
    output the current strongly connected component
      end if
            end function*/

    /*for each v in V do
      if (v.index is undefined) then
    strongconnect(v)
    end if
          end for*/

    /*possible memory `leak` as it is not `tailrec`,
    `stack` may grow infinitely
    */
    /*
    must find & return SCC if any
     */
    //@scala.annotation.tailrec
    def strongConnect(
                       nodeKeyToCheck: Int /*,
                     currentResultingSCC: List[Int] =
                     List.empty*/
                       ): Int = {
      //): List[Int] = {
      //assume(adjacencyList.get(nodeKeyToCheck).isDefined)
      //assume(adjacencyList.isDefinedAt(nodeKeyToCheck))

      val nodeVal:
      //NodeFieldsArray =
      NodeMapValFieldsDynamic =
        adjacencyList //(nodeKeyToCheck)
        .get(nodeKeyToCheck)
        .get
      /*side effects*/
      nodeVal.nodeIndex =
        localIndex
      //Some(localIndex)
      nodeVal.nodeLowLink = localIndex
      localIndex += 1
      localStack = nodeKeyToCheck +: localStack
      //nodeVal.isInStack = true
      nodesOnStack = nodesOnStack + nodeKeyToCheck

      /* iterate over ? adjusted ?*/
      /*calculate ? min val for `lowlink`?*/
      /*for {
        adjustedNodeKey <- nodeVal.adjustedNodes
      }*/
      nodeVal
      .adjustedNodes
      .foreach(adjustedNodeKey => {
        //assume(adjacencyList.get(adjustedNodeKey).isDefined)
        val adjustedNodeVal:
        //NodeFieldsArray =
        NodeMapValFieldsDynamic =
          adjacencyList //(adjustedNodeKey)
          .get(adjustedNodeKey).get

        if (
          adjustedNodeVal.nodeIndex == -1
        //.isEmpty /* is undefined*/
        //nodesOnStack.contains()
        ) {
          // has not yet been visited
          // recurse on it
          strongConnect(adjustedNodeKey)
          /*side effect*/
          nodeVal.nodeLowLink =
            min(nodeVal.nodeLowLink, adjustedNodeVal.nodeLowLink)
        } else if (
               //adjustedNodeVal.nodeIndex.isDefined &&
               //adjustedNodeVal.isInStack
                 nodesOnStack.contains(adjustedNodeKey)
               ) {
          // Successor w is in stack S and hence in the current SCC
          nodeVal.nodeLowLink =
            min(nodeVal.nodeLowLink,
                /*careful with '-1' may be use 'Int.MaxValue' as default*/
                adjustedNodeVal.nodeIndex
                //.getOrElse(nodeVal.nodeLowLink)
               )
        }
      }
              )

      // If it is a `root` node,
      // pop the `stack` and
      // generate an `SCC`
      //assume(nodeVal.nodeIndex.isDefined)
      if (nodeVal.nodeLowLink == nodeVal.nodeIndex
      //.get
      ) {
        //start /  create a new `strongly connected component`
        //`until` loop with post condition,
        // so always run at least once
        @scala.annotation.tailrec
        def loopOnStack(
                         //loopSCC: List[Int] = List.empty,
                         SCCsize: Int = 0,
                         condition: Boolean = false
                         ): Int = {
          //): List[Int] = {
          if (
            condition ||
              localStack.isEmpty
          ) {
            /*stop & return / exit */
            /*side effect*/
            graphSCCs =
              //loopSCC.length +:
              SCCsize +:
                graphSCCs
            //loopSCC +: graphSCCs
            //currentResultingSCC
            //loopSCC
            SCCsize
          } else /*if (localStack.nonEmpty)*/ {
            val stackTopKey = localStack.head
            /*side effect*/
            localStack = localStack.tail
            //assume(adjacencyList.get(stackTopKey).isDefined)
            /*val stackTopVal: NodeMapValFieldsDynamic =
              adjacencyList.get(stackTopKey).get*/
            /*side effect*/
            //stackTopVal.isInStack = false
            nodesOnStack = nodesOnStack - stackTopKey
            /*val updatedSCC: List[Int] =
              stackTopKey +: loopSCC*/
            val updatedSCCsize: Int =
              SCCsize + 1

            /*post condition*/
            if (stackTopKey == nodeKeyToCheck) {
              /*stop & return / exit */
              //currentResultingSCC
              //loopSCC
              loopOnStack(
                           //updatedSCC,
                           updatedSCCsize,
                           condition = true)
            } else {
              /*recursion*/
              loopOnStack(
                           //updatedSCC,
                           updatedSCCsize,
                           condition = condition)
            }
          }
        }
        /*return value*/
        /*initialization*/
        loopOnStack()
      } else {
        /*return value*/
        //List.empty
        0
      }
    }

    /*side effect*/
    /*adjacencyList
    .foreach(
        //(nodeKey: Int, nodeVal: NodeMapValFieldsDynamic) =>
        //((nodeKey, nodeVal): (Int,NodeMapValFieldsDynamic)) =>
        n => {
          val (nodeKey, nodeVal): (Int, NodeMapValFieldsDynamic) = n
          if (nodeVal.isExplored) {
            strongConnect(nodeKey)
          }
        }
            )*/
    /*return*/
    /*for {
      (nodeKey, nodeVal) <- adjacencyList
      if nodeVal.nodeIndex.isEmpty
    } yield strongConnect(nodeKey)*/
    adjacencyList
    .foreach(
    /*(nodeKey, nodeVal) =>
  if (nodeVal.nodeIndex.isEmpty) {
    strongConnect(nodeKey)
  }*/ {
      /*case NodeFieldsArray(
      nodeKey,
      nodeIndex,
      nodeLowLink,
      adjustedNodes) if nodeIndex == -1*/
      case (nodeKey, nodeVal) if nodeVal.nodeIndex == -1
        //.isEmpty
             =>
        strongConnect(nodeKey)
      case _ =>
    }
            )
    /*return value*/
    graphSCCs
  }

  /*
  TODO
  Done: replace 'Set' with 'List' or 'Stream' & check the effect
  - 'Stream' take a little less space then 'Set' according to the test
  if fails,
  replace 'Map' with 'Array',
  as it's size known ahead ('nodes' are just a range from '1' to '875714') &
  'Array' is a native Java object
  and, if it is possible, `tailrec` 'strongConnect' or
  try to refactor / decompose to that case,
  because here is / lays a main impact on memory leak
   */
  //input: graph G = (V, E)
  //output: set of strongly connected components (sets of vertices)
  def tarjanForDiGraphArray(
                             adjacencyList:
                             Array[NodeFieldsArray]
                             //ArrayBuffer[NodeFieldsArray]
                             //Map[Int, Set[Int]]
                             //Map[Int, NodeMapValFieldsDynamic] //,
                             /*index: Int = 0,
                             stack:
                             List[Int] =*/
                             //List[IndexedNode] =
                             //List[Map[Int, NodeMapValFieldsDynamic]] =
                             //List.empty
                             ): Stream[Int] = {
    //): List[Int] = {
    //): List[List[Int]] = {
    //): Iterable[List[Int]] = {
    //): Vector[List[Int]] = {
    var localIndex: Int = 0 //index
    var nodesOnStack: BitSet =
      BitSet.empty
    var graphSCCs:
    //List[List[Int]] =
    /*List[Int] =
      List.empty*/
    Stream[Int] =
      Stream.empty
    var localStack:
    List[Int] =
    //List[IndexedNode] =
    //stack
      List.empty
    //index := 0
    //S := empty

    /*function strongconnect(v)
    // Set the depth index for v to the smallest unused index
    v.index := index
    v.lowlink := index
    index := index + 1
    S.push(v)
    v.onStack := true

    // Consider successors of v
    for each (v, w) in E do
      if (w.index is undefined) then
    // Successor w has not yet been visited; recurse on it
    strongconnect(w)
    v.lowlink  := min(v.lowlink, w.lowlink)
    else if (w.onStack) then
    // Successor w is in stack S and hence in the current SCC
    v.lowlink  := min(v.lowlink, w.index)
    end if
          end for

    // If v is a root node, pop the stack and generate an SCC
    if (v.lowlink = v.index) then
    start a new strongly connected component
    repeat
    w := S.pop()
    w.onStack := false
    add w to current strongly connected component
    until (w = v)
    output the current strongly connected component
      end if
            end function*/

    /*for each v in V do
      if (v.index is undefined) then
    strongconnect(v)
    end if
          end for*/

    /*
    TODO switch from a linear recursive process to a linear iterative process
    that has a fixed number of `state variables`
     */
    /*possible memory `leak` as it is not `tailrec`,
    `stack` may grow infinitely
    */
    /*
    must find & return SCC if any
     */
    //@scala.annotation.tailrec
    def strongConnect(
                       nodeKeyToCheck: Int /*,
                     currentResultingSCC: List[Int] =
                     List.empty*/
                       ): Int = {
      //): List[Int] = {
      //assume(adjacencyList.get(nodeKeyToCheck).isDefined)
      //assume(adjacencyList.isDefinedAt(nodeKeyToCheck))
      /*!!!Warn!!!: 'DiGraphArray' has `empty` '0' element, just for shift*/
      val nodeVal:
      NodeFieldsArray =
      //NodeMapValFieldsDynamic =
        adjacencyList(nodeKeyToCheck)
      /*.get(nodeKeyToCheck)
      .get*/
      /*side effects*/
      nodeVal.nodeIndex =
        localIndex
      //Some(localIndex)
      nodeVal.nodeLowLink = localIndex
      localIndex += 1
      localStack = nodeKeyToCheck +: localStack
      //nodeVal.isInStack = true
      nodesOnStack = nodesOnStack + nodeKeyToCheck

      /* iterate over ? adjusted ?*/
      /*calculate ? min val for `lowlink`?*/
      /*for {
        adjustedNodeKey <- nodeVal.adjustedNodes
      }*/
      nodeVal
      .adjustedNodes
      .foreach(adjustedNodeKey => {
        //assume(adjacencyList.get(adjustedNodeKey).isDefined)
        val adjustedNodeVal:
        NodeFieldsArray =
        //NodeMapValFieldsDynamic =
          adjacencyList(adjustedNodeKey)
        //.get(adjustedNodeKey).get

        if (
          adjustedNodeVal.nodeIndex == -1
        //.isEmpty /* is undefined*/
        //nodesOnStack.contains()
        ) {
          // has not yet been visited
          // recurse on it
          strongConnect(adjustedNodeKey)
          /*side effect*/
          nodeVal.nodeLowLink =
            min(nodeVal.nodeLowLink, adjustedNodeVal.nodeLowLink)
        } else if (
               //adjustedNodeVal.nodeIndex.isDefined &&
               //adjustedNodeVal.isInStack
                 nodesOnStack.contains(adjustedNodeKey)
               ) {
          // Successor w is in stack S and hence in the current SCC
          nodeVal.nodeLowLink =
            min(nodeVal.nodeLowLink,
                /*careful with '-1' may be use 'Int.MaxValue' as default*/
                adjustedNodeVal.nodeIndex
                //.getOrElse(nodeVal.nodeLowLink)
               )
        }
      }
              )

      // If it is a `root` node,
      // pop the `stack` and
      // generate an `SCC`
      //assume(nodeVal.nodeIndex.isDefined)
      if (nodeVal.nodeLowLink == nodeVal.nodeIndex
      //.get
      ) {
        //start /  create a new `strongly connected component`
        //`until` loop with post condition,
        // so always run at least once
        @scala.annotation.tailrec
        def loopOnStack(
                         //loopSCC: List[Int] = List.empty,
                         SCCsize: Int = 0,
                         condition: Boolean = false
                         ): Int = {
          //): List[Int] = {
          if (
            condition ||
              localStack.isEmpty
          ) {
            /*stop & return / exit */
            /*side effect*/
            graphSCCs =
              //loopSCC.length +:
              SCCsize +:
                graphSCCs
            //loopSCC +: graphSCCs
            //currentResultingSCC
            //loopSCC
            SCCsize
          } else /*if (localStack.nonEmpty)*/ {
            val stackTopKey = localStack.head
            /*side effect*/
            localStack = localStack.tail
            //assume(adjacencyList.get(stackTopKey).isDefined)
            /*val stackTopVal: NodeMapValFieldsDynamic =
              adjacencyList.get(stackTopKey).get*/
            /*side effect*/
            //stackTopVal.isInStack = false
            nodesOnStack = nodesOnStack - stackTopKey
            /*val updatedSCC: List[Int] =
              stackTopKey +: loopSCC*/
            val updatedSCCsize: Int =
              SCCsize + 1

            /*post condition*/
            if (stackTopKey == nodeKeyToCheck) {
              /*stop & return / exit */
              //currentResultingSCC
              //loopSCC
              loopOnStack(
                           //updatedSCC,
                           updatedSCCsize,
                           condition = true)
            } else {
              /*recursion*/
              loopOnStack(
                           //updatedSCC,
                           updatedSCCsize,
                           condition = condition)
            }
          }
        }
        /*return value*/
        /*initialization*/
        loopOnStack()
      } else {
        /*return value*/
        //List.empty
        0
      }
    }

    /*side effect*/
    /*adjacencyList
    .foreach(
        //(nodeKey: Int, nodeVal: NodeMapValFieldsDynamic) =>
        //((nodeKey, nodeVal): (Int,NodeMapValFieldsDynamic)) =>
        n => {
          val (nodeKey, nodeVal): (Int, NodeMapValFieldsDynamic) = n
          if (nodeVal.isExplored) {
            strongConnect(nodeKey)
          }
        }
            )*/
    /*return*/
    /*for {
      (nodeKey, nodeVal) <- adjacencyList
      if nodeVal.nodeIndex.isEmpty
    } yield strongConnect(nodeKey)*/
    adjacencyList
    .foreach(
    /*(nodeKey, nodeVal) =>
  if (nodeVal.nodeIndex.isEmpty) {
    strongConnect(nodeKey)
  }*/ {
      case NodeFieldsArray(
      nodeKey,
      nodeIndex,
      nodeLowLink,
      adjustedNodes) if nodeIndex == -1 && nodeKey != -1
        //case (nodeKey, nodeVal) if (nodeVal.nodeIndex == -1
        //.isEmpty
             =>
        strongConnect(nodeKey)
      case _ =>
    }
            )
    /*return value*/
    graphSCCs
  }

  //input: graph G = (V, E)
  //output: set of strongly connected components (sets of vertices)
  def tarjanForDiGraphDyn_ver2(
                                adjacencyList: Map[Int,
                                  NodeMapValFieldsDynamic],
                                index: Int = 0,
                                stack:
                                List[IndexedNode] =
                                //List[Map[Int, NodeMapValFieldsDynamic]] =
                                List.empty
                                ): Vector[List[Int]] = {
    //): List[List[Int]] = {
    /*global 'index' tracker*/
    //var index: Int = 0
    //S := empty
    /*
    Use 'List' instead:
    'stack.push' 'x' becomes
    'x :: list'
    'stack.pop' is 'list.tail'
     */
    //val stack /*: Stack[Nothing]*/ =
    //scala.collection.immutable.Stack.empty[IndexedNode]
    //List.empty[IndexedNode]

    /*auxiliary method*/
    def createNewSCC(
                      newSCC: List[Int] =
                      List.empty,
                      nodeFromStack:
                      Option[IndexedNode] =
                      //Option[NodeMapValFieldsDynamic] =
                      //Option[Map[Int,NodeMapValFieldsDynamic]] =
                      None,
                      /*assume that 'node.inStack'*/
                      currentStack:
                      /*Stack*/
                      List[IndexedNode],
                      //List[NodeMapValFieldsDynamic],
                      //List[Map[Int,NodeMapValFieldsDynamic]],
                      //nodeKey: Int,
                      //nodeFields: NodeMapValFieldsDynamic
                      node: IndexedNode
                      ): List[Int] = {
      if (
        nodeFromStack.isDefined
      ) {
        /*deconstruct*/
        /*val (stack_Key,stack_Val):(Int,NodeMapValFieldsDynamic) =
          nodeFromStack.get.head*/

        if (
        //stack_Val == nodeFields
          nodeFromStack.get.nodeVal == node.nodeVal
        ) {
        } else {

        }
        /*return value*/
        newSCC.reverse
      } else {
        /*val (w, stackWithoutTop): (IndexedNode, Stack[IndexedNode]) =
          stack.pop*/
        val stackWithoutTop = currentStack.tail
        /*val (topKey,topVal):(Int,NodeMapValFieldsDynamic) =
          currentStack
          .head
          .head*/
        val topNode: IndexedNode = currentStack.head
        /*side effect*/
        //topVal.isInStack = false
        topNode.isInStack = false
        /*recursion*/
        createNewSCC(
                      newSCC =
                        //topKey
                        topNode.nodeVal +:
                          newSCC,
                      nodeFromStack =
                        Some(topNode),
                      //Some(Map(topKey->topVal)),
                      currentStack = stackWithoutTop,
                      //nodeKey = nodeKey,
                      //nodeFields = nodeFields
                      node = node
                    )
      }
    }

    /*recursive method, what is the output, except side effects ?*/
    /*changes 'index', so must return new value*/
    /*create discovered 'SCC', so must return it*/
    /*changes 'stack', so must return it*/
    def strongConnect(
                       /*?just 'key'?*/
                       node:
                       Int,
                       //IndexedNode,
                       index: Int
                       ): SCC_Result = {
      //): List[Int] = {
      /*auxiliary method*/
      /*trying to find `min` val*/
      def minLowLink(
                      //nodeAndAdjusted: IndexedNodeWithAdjacencyList,
                      adjustedNodes:
                      Stream[Int],
                      //Set[Int],
                      //List[IndexedNode],
                      /*actually started with 'node.nodeLowLink'*/
                      currentMin: Int = Int.MaxValue,
                      node:
                      IndexedNode,
                      //Map[Int, NodeMapValFieldsDynamic],
                      /*global index state changing with time*/
                      index: Int
                      ): Int = {
        /*val (nodeKey, nodeVal): (Int, NodeMapValFieldsDynamic) =
          node.head*/

        if (adjustedNodes.isEmpty) {
          /*return value*/
          currentMin
        } else {
          val adjNode:
          IndexedNode =
          //NodeMapValFieldsDynamic =
          //adjustedNodes.head
          //NodeMapValFieldsDynamic(
            IndexedNode(
                         nodeVal = adjustedNodes.head,
                         //isExplored = true,
                         nodeIndex = None,
                         nodeLowLink = adjustedNodes.head,
                         isInStack = false //,
                         //doneOrder = None,
                         //groupLeader = None,
                         //adjustedNodes = adjustedNodes
                       )

          /*? partial function ?*/
          //val newMin: Int =
          if (adjNode.nodeIndex.isEmpty) {
            // Successor 'w'
            // has not yet been visited recurse on it
            /*outer recursion*/
            /*?must change 'node.nodeLowLink'?*/
            val SCC_Result(newLowlink, newIndex, newStack, newSCC) =
              strongConnect(
                             //adjNode,
                             adjNode.nodeVal,
                             //nodeKey,
                             //nodeVal,
                             /*?must return as global state?*/
                             //newIndex
                             index
                           )
            /*side effect*/
            node
            //nodeVal
            .nodeLowLink = newLowlink
            /*recursion*/
            minLowLink(
                        adjustedNodes =
                          /*must converge eventually*/
                          adjustedNodes.tail,
                        currentMin =
                          newLowlink.min(adjNode.nodeLowLink),
                        //node.nodeLowLink.min(adjNode.nodeLowLink),
                        node = node,
                        newIndex
                      )
            /*return value*/
            //node.nodeLowLink.min(adjNode.nodeLowLink)
          } else if (adjNode.isInStack) {
            // Successor 'w' is in `stack` S and
            // hence in the current `SCC`
            /*recursion*/
            minLowLink(
                        adjustedNodes =
                          /*must converge eventually*/
                          adjustedNodes.tail,
                        currentMin =
                          node
                          //nodeVal
                          .nodeLowLink
                          .min(
                              adjNode
                              .nodeIndex
                              .getOrElse(
                                  node
                                  //nodeVal
                                  .nodeLowLink)),
                        node = node,
                        index
                      )
            /*return value*/
            //node.nodeLowLink.min(adjNode.nodeIndex.getOrElse(node
            // .nodeLowLink))
          } else {
            /*return value*/
            //currentMin
            /*recursion*/
            minLowLink(
                        adjustedNodes =
                          /*must converge eventually*/
                          adjustedNodes.tail,
                        currentMin =
                          currentMin,
                        node = node,
                        index
                      )
          }
          /*recursion*/
          /*minLowLink(
                      adjustedNodes=
                        /*must converge eventually*/
                        adjustedNodes.tail,
                      currentMin=
                        newMin,
                      node: IndexedNode,
                      index
                    )*/
        }
      }

      // Set the depth index for 'node' to the smallest unused `index`
      /*initialization*/
      /*side effects*/
      //node.nodeIndex = Some(index)
      /*?And what is initial value +infinity?*/
      //node.nodeLowLink = index
      /*change state*/
      //index = index + 1
      val newIndex = index + 1
      //stack.push(node)
      val vInStack:
      //List[Map[Int, NodeMapValFieldsDynamic]] =
      List[IndexedNode] =
      //node +:
      //NodeMapValFieldsDynamic(
        IndexedNode(
                     nodeVal = node,
                     nodeIndex = None,
                     nodeLowLink = node,
                     isInStack = false
                   ) +:
          stack
      /*side effect*/
      //node.inStack = true

      /*must be separate method*/
      // Consider `successors` of 'node'
      //for each(node, w) in E do
      //val nodeAndAdjusted: IndexedNodeWithAdjacencyList =
      //adjacencyList(node.nodeVal - 1)
      val adjustedNodesKeys:
      //Set[Int] =
      Stream[Int] =
        adjacencyList.get(
                           node
                           //.nodeVal
                         ).get.adjustedNodes
      /*trying to find `min` val*/
      //node.nodeLowLink =
      minLowLink(
                  adjustedNodes =
                    //nodeAndAdjusted.adjustedNodes,
                    adjustedNodesKeys,
                  currentMin =
                    node,
                  //node.nodeLowLink,
                  node =
                    IndexedNode(
                                 nodeVal = node,
                                 nodeIndex = None,
                                 nodeLowLink = node,
                                 isInStack = false
                               ),
                  /*? may change within ?*/
                  index = newIndex
                )
      //(
      /*for {
        adjNode <- nodeAndAdjusted.adjustedNodes
      } yield
      /*? partial function ?*/
      //if (w.index is undefined) then
        if (adjNode.nodeIndex.isEmpty) {
          // Successor 'w'
          // has not yet been visited recurse on it
          /*recursion*/
          strongConnect(
                         adjNode,
                        /*?must return as global state?*/
                        newIndex
                       )
          /*side effect*/
          node.nodeLowLink =
            node.nodeLowLink.min(adjNode.nodeLowLink)
          /*return*/
          node.nodeLowLink
        } else if (adjNode.inStack) {
          // Successor 'w' is in `stack` S and
          // hence in the current `SCC`
          /*side effect*/
          node.nodeLowLink =
            node.nodeLowLink
            .min(adjNode.nodeIndex.getOrElse(node.nodeLowLink))
          /*return*/
          node.nodeLowLink
        }*/
      //).head

      // If 'node' is a `root` node,
      // 'pop' the `stack` and
      // generate an `SCC`
      /*assume that 'node.nodeIndex.isDefined'*/
      if (
      //node.nodeLowLink == node.nodeIndex.get
        true
      ) {
        //start a new strongly connected component
        //val newSCC: List[Int] = List.empty
        //repeat
        /*val (w, stackWithoutTop): (IndexedNode, Stack[IndexedNode]) =
          stack.pop
        w.inStack = false*/
        //add 'w' to `current` `strongly connected component`
        //w +: newSCC
        //until(w = node)
        //output the current strongly connected component

        /*return value*/
        //newSCC.reverse
        SCC_Result(
                    //node.nodeLowLink,
                    node,
                    newIndex,
                    vInStack,
                    createNewSCC(
                                  newSCC =
                                    List.empty,
                                  nodeFromStack = None,
                                  currentStack = vInStack,
                                  //nodeFields =
                                  node =
                                    IndexedNode(
                                                 nodeVal = node,
                                                 nodeIndex = None,
                                                 nodeLowLink = node,
                                                 isInStack = false
                                               )
                                )
                  )
      } else {
        /*return value*/
        SCC_Result(
                    //node.nodeLowLink,
                    node,
                    newIndex,
                    vInStack,
                    List.empty[Int]
                  )
      }
    }

    /*instead ?must? call recursion on itself*/
    /*return value*/
    (for {
    //adjustedNode <- adjacencyList.valuesIterator //G.V
      adjustedNode <- adjacencyList.keysIterator
    /*guard*/
    //if (node.index is undefined)
    //if node.node.nodeIndex.isEmpty
    } yield strongConnect(
                           //nodeFields.node,
                           adjustedNode,
                           /*incremented every iteration within method call*/
                           index
                         ).sCC).toVector
  }

}
