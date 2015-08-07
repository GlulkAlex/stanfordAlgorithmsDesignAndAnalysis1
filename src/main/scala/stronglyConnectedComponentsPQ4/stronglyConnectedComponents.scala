package stronglyConnectedComponentsPQ4

import scala.math.log
import scala.collection.immutable.Queue
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

  /*node's 'rank' or 'layer'*/
  case class RankedNode(node: Int, rank: Int)

  case class Edge(
                   startNode: Int,
                   endNode: Int)

  /*directed edge*/
  case class Arc(
                  tail: Int,
                  head: Int) {
    override def toString = s">>$tail-->$head}"
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
                           5819251(number of edges doubled?) did not equal 875714
                           * */
                           /*nodes: Vector[Int] =
                           Vector.empty[Int]*/
                             nodes: Seq[Int] =
                           Seq.empty[Int],
                           arcs: Vector[Arc] =
                           Vector.empty[Arc],
                           lastTail: Option[Int] = None/*,
                           minNode: Int = Double.PositiveInfinity,
                             maxNode: Int = Double.NegativeInfinity*/
                           //): (Vector[Int], Vector[Arc]) = {
                           ): DirectedGraph = {
    if (fileContentIter.isEmpty) {
      /*return value*/
      //(nodes, arcs)
      //DirectedGraph(nodes, arcs)
      //DirectedGraph(minNode to maxNode, arcs)
      DirectedGraph(
                     nodes.distinct,
                     arcs)
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
          Arc(tail, head) +: arcs
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
      val newNodes: Seq[Int] =
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
        if (
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
        }
      /*val addHead: Vector[Int] =
        if (
          tail == head ||
            lastTail.contains(head)
        ) {
          addTail
        } else {
          head +: addTail
        }*/
      /*recursion*/
      extractArcsAndNodes(
                           /*reduced already by '.next()'*/
                           fileContentIter,
                           newNodes,
                           //addHead,
                           newArcs,
                           Some(tail)
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
      for {arc <- arcs} yield arc.tail
    val heads: Vector[Int] =
      for {arc <- arcs} yield arc.head
    val nodes: Vector[Int] =
      tails.union(heads).distinct
    /*return value*/
    nodes
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
                  a.tail == startingNode &&
                    !exploredNodes.contains(a.head))
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
      val (currentTail, currentHead): (Int, Int) /*Arc*/ =
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
        }
      /*add to ranked / explored*/
      val currentlyRanked: Vector[RankedNode] =
        if (
          rankedNodes.isEmpty ||
            currentArc.tail == currentArc.head
        ) {
          /*?side effect?*/
          RankedNode(currentArc.tail, 0) +: rankedNodes
        } else {
          /*find 'tail'.rank*/
          //val tailRank: Int =
          val rankedTail: RankedNode =
            rankedNodes
            .find(_.node == currentArc.tail)
            .getOrElse(RankedNode(currentArc.tail, 0))
          /*return value*/
          RankedNode(currentArc.tail, rankedTail.rank + 1) +: rankedNodes
        }
      /*layer / rank of 'head' must be 'tail'.rank + 1*/
      /*!may be time consuming because of nested loop!*/
      val allArcsFromLastExplored: /*List*/ Vector[Arc] =
      //graph
      //.collect(pf match {case x if x.})
      /*.filter(a =>
                a.tail == currentArc.tail &&
                  !rankedNodes
                   .contains(a.head)
             )*/
        for {
          arc <- graph if arc.tail == currentArc.tail
          node <- rankedNodes if arc.head != node.node
        } yield arc
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
                 rankedNodes
               )
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

}
