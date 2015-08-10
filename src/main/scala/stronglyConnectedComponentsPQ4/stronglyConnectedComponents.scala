package stronglyConnectedComponentsPQ4

import scala.math.log
import scala.math.min
import scala.math.max
import scala.collection.immutable.Queue
import scala.collection.immutable.Stack
import randomGenerators.RandomGenerators.randomIntWithinInterval

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

  trait TraverseDirection

  case object Up extends TraverseDirection

  case object Down extends TraverseDirection

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

  case class DepthFirstSearchResult(
                                     /*preOrder: List[IsExploredNode],
                                     postOrder: List[IsExploredNode]*/
                                     preOrder: Stream[IsExploredNode],
                                     postOrder: Stream[IsExploredNode]
                                     )

  /*node's 'rank' or 'layer'*/
  case class IndexedNode(
                          nodeVal: Int,
                          var nodeIndex: Option[Int],
                          var nodeLowLink: Int,
                          var inStack: Boolean
                          )

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
      s"""${node}>to>${adjustedNodes.mkString("", ">", "")}"""
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
                           IsExploredNode(arcsRemains.head.arcTail, false),
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
                           IsExploredNode(currentNodeVal + 1, false),
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
                                           nodes: Vector[IsExploredNode] =
                                           Vector.empty,
                                           /*for use 'head','tail'*/
                                           /*may be redundant*/
                                           nodesRemains:
                                           List[IsExploredNode] =
                                           List.empty,
                                           /*at start equal 'minNodeVal'
                                            - '1'*/
                                           /*counter of added 'nodes'*/
                                           /*must exceed 'maxNodeVal'
                                           eventually*/
                                           /*may be redundant*/
                                           currentNodeVal: Int,
                                           /*must be empty eventually*/
                                           arcsRemains: List[Arc],
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
                                           /*rangeSize*/
                                           nodesAmount: Int = 1,
                                           nodeIndexShift: Int = -1
                                           ):
  Vector[ExplorableNodeWithAdjusted] = {
    if (
      nodesRemains.isEmpty
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
                                      nodes(arcsRemains.head.arcHead - 1)
                                  )
      val newCurrentNodeVal =
        currentNodeVal
      //nodesRemains.head.node
      val newArcsRemains = arcsRemains.tail
      val newAdjustedNodes = adjustedNodes

      /*recursion*/
      makeExplorableAdjacencyListFromArcs(
                                           nodes = nodes,
                                           nodesRemains = nodesRemains,
                                           currentNodeVal =
                                             newCurrentNodeVal,
                                           arcsRemains =
                                             newArcsRemains,
                                           adjustedNodes =
                                             newAdjustedNodes,
                                           nodeToAdd =
                                             Some(newNodeToAdd),
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
      val newAdjustedNodes: Vector[ExplorableNodeWithAdjusted] =
        if (nodeToAdd.isEmpty) {
          adjustedNodes
        } else /*if (nodeToAdd.isDefined)*/ {
          adjustedNodes :+ nodeToAdd.get
        }
      /*val nodeToAddValue =
        nodeToAdd.get*/
      val newNodeToAdd: ExplorableNodeWithAdjusted =
        ExplorableNodeWithAdjusted(
                                    nodes(arcsRemains.head.arcTail - 1),
                                    //List(nodes(arcsRemains.head.arcHead - 1))
                                    Stream(nodes(arcsRemains.head.arcHead - 1))
                                  )
      val newCurrentNodeVal =
        currentNodeVal + 1
      //nodesRemains.head.node
      //nodesRemains.head.nodeVal
      val newArcsRemains = arcsRemains.tail

      /*recursion*/
      makeExplorableAdjacencyListFromArcs(
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
                                             Some(newNodeToAdd),
                                           nodeIndexShift = nodeIndexShift
                                         )
    } else /*if (
             currentNodeVal < maxNodeVal &&
               (arcsRemains.isEmpty ||
               (arcsRemains.head.arcTail >= currentNodeVal + 1 ))
           )*/ {
      //next 'nodeVal'
      //=>add 'nodeToAdd' if any to 'adjustedNodes'
      val newAdjustedNodes: Vector[ExplorableNodeWithAdjusted] =
        if (nodeToAdd.isEmpty) {
          adjustedNodes
        } else {
          adjustedNodes :+ nodeToAdd.get
        }
      val newNodeToAdd: ExplorableNodeWithAdjusted =
        ExplorableNodeWithAdjusted(
                                    nodes(currentNodeVal),
                                    //nodes(currentNodeVal + nodeIndexShift),
                                    //nodes(nodesRemains.head.node),
                                    //List()
                                    Stream.empty
                                  )
      val newCurrentNodeVal =
        currentNodeVal + 1
      //nodesRemains.head.node
      val newArcsRemains = arcsRemains

      /*recursion*/
      makeExplorableAdjacencyListFromArcs(
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
                                             Some(newNodeToAdd),
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
    .map(IsExploredNode(_, false))
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

  /*Depth-first search (DFS)*/
  //A recursive implementation of DFS
  //Input: A graph 'G' and a (starting) vertex 'v' of 'G'
  //Output: All `vertices` reachable from 'v', labeled as discovered
  /*how to refactor this as 'tailrec' ?*/
  def postOrderDFS(
                    /*G*/ graph: Vector[ExplorableNodeWithAdjusted],
                    /*start node*/
                    v: Int,
                    /*nodeToCheck: Option[ExplorableNodeWithAdjusted] =
                    None,*/
                    /*accum*/
                    /*exploredNodes: List[IsExploredNode] =
                    List.empty*/
                    exploredNodes: Stream[IsExploredNode] =
                    Stream.empty,
                    nodeIndexShift: Int = -1
                    ): Stream[IsExploredNode] = {
    //): List[IsExploredNode] = {
    //label 'v' as `discovered`
    val starNode: ExplorableNodeWithAdjusted =
      graph(v + nodeIndexShift)

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
        /*same value for 'preOrder'*/
        //exploredNodes
        exploredNodes :+ starNode.node
      } else {
        //val exploredNodesUpdated: List[IsExploredNode] =
        val exploredNodesUpdated: Stream[IsExploredNode] =
          if (adjacentEdges.head.isExplored) {
            /*skip current 'node', check next*/
            /*same value for 'preOrder'*/
            exploredNodes
          } else /*if (!adjacentEdges.head.isExplored)*/ {
            /*outer recursion*/
            postOrderDFS(
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
  //Input: A graph 'G' and a (starting) vertex (node) 'v' of 'G'
  //Output: number of `nodes` reachable from 'v', labeled as discovered
  def maxPostOrderDFS(
                       graph: Vector[ExplorableNodeWithAdjusted],
                       /*start node*/
                       v: Int,
                       exploredNodes: Int =
                       0,
                       nodeIndexShift: Int = -1
                       ): Int = {
    //label 'v' as `discovered`
    val starNode: ExplorableNodeWithAdjusted =
      graph(v + nodeIndexShift)

    @scala.annotation.tailrec
    def innerLoop(
                   //adjacentEdges: List[IsExploredNode],
                   adjacentEdges: Stream[IsExploredNode],
                   /*accum*/
                   exploredNodesResult: Int
                   ): Int = {
      if (adjacentEdges.isEmpty) {
        /*return value*/
        /*same value for 'preOrder'*/
        //exploredNodesResult
        exploredNodesResult + 1
      } else {
        val exploredNodesUpdated: Int =
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
                                  postExploredNodes//,
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

  /*Reverse the directions of all `arcs` to obtain the `transpose` `graph`.*/
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
                                resultAccum.postOrder//,
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
      val currentNode =
        graph(indexCounter)
      /*cases:
      * either new 'nodes' added or not (same `explored` list)*/
      /*?no 'concat' / 'union'?*/
      //val resultAccumPostOrderUpdated: List[IsExploredNode] =
      val resultAccumPostOrderUpdated: Stream[IsExploredNode] =
        if (!currentNode.node.isExplored) {
          /*new*/
          postOrderDFS(
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
                                    preOrderRemains: Stream[IsExploredNode],
                                    resultAccum: Stream[Stream[IsExploredNode]] =
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
      val updatedSCCsResult: Stream[Stream[IsExploredNode]] =
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
                                        graph:
                                        Vector[ExplorableNodeWithAdjusted],
                                        /*check order and termination
                                        condition*/
                                        /*must be reset as `unExplored`*/
                                        //postOrderRemains:
                                        // List[IsExploredNode],
                                        postOrderRemains:
                                        Stream[IsExploredNode],
                                        /*resultAccum: List[Int] =
                                        List.empty,*/
                                        resultAccum: Stream[Int] =
                                        Stream.empty,
                                        graphLength: Int,
                                        indexCounter: Int = 0,
                                        nodesValuesZeroBased: Boolean = false,
                                        minNodeVal: Int = 1,
                                        nodeIndexShift: Int = -1
                                        ): Stream[Int] = {
    if (
    //indexCounter >= graphLength
      postOrderRemains.isEmpty
    ) {
      /*return value*/
      resultAccum
    } else {
      val currentNode: ExplorableNodeWithAdjusted =
      //graph(indexCounter)
        graph(postOrderRemains.head.node + nodeIndexShift)
      /*cases:
      * either new 'nodes' added or not (same `explored` list)*/
      /*?no 'concat' / 'union'?*/
      //val newSCC: List[IsExploredNode] =
      val updatedSCCsResult: Stream[Int] =
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
                                        graphLength = graphLength,
                                        indexCounter = indexCounter + 1,
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
        w.inStack = false
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
          } else if (adjNode.inStack) {
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
      v.inStack = true

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
      if (v.node.nodeIndex.isEmpty)
    } yield strongConnect(
                           v.node,
                           /*incremented every iteration within method call*/
                           index
                         ).sCC
  }

}
