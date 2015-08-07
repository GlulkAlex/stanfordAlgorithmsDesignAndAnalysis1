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

  case class ComponentResult(
                              nodesAmount: Int,
                              exploredNodes: Vector[Int])

  /*node's 'rank' or 'layer'*/
  case class IsExploredNode(
                             node: Int,
                             var isExplored: Boolean
                             )
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
          RankedNode(currentArc.head, 0) +: rankedNodes
        } else if (
                 rankedNodes.nonEmpty &&
                   currentArc.tail == currentArc.head
               ) {
          /*self loops skip*/
          /*unchanged*/
          rankedNodes
        } else /*if (
            rankedNodes.nonEmpty &&
              currentArc.tail != currentArc.head
          )*/ {
          if (rankedNodes.exists(_.node == currentArc.head)) {
            /*unchanged*/
            rankedNodes
          } else {
            /*find 'tail'.rank*/
            //val tailRank: Int =
            val rankedTail: RankedNode =
              rankedNodes
              //.find(_.node == currentArc.head)
              /*`parent` 'node'*/
              .find(_.node == currentArc.tail)
              /*must never occur*/
              .getOrElse(RankedNode(currentArc.tail, 0))
            /*return value*/
            RankedNode(currentArc.head, rankedTail.rank + 1) +: rankedNodes
          }
        }
      /*layer / rank of 'head' must be 'tail'.rank + 1*/
      /*!may be time consuming because of nested loop!*/
      val allArcsFromLastExplored: /*List*/ Vector[Arc] =
        graph
        .collect(
        { case a if (
          /*start with explored 'node'*/
          a.tail == currentArc.head &&
            //!rankedNodes.unzip._1.contains(a.head)
            /*where 'arc's 'head' yet unexplored*/
            !rankedNodes.exists(_.node == a.head)
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
          (currentArc.head +: exploredNodesInSCC, nodesCounter + 1)
        } else if (
                 (nodesCounter > 0 ||
                   exploredNodesInSCC.nonEmpty) &&
                   currentArc.tail == currentArc.head
               ) {
          /*self loops skip*/
          /*unchanged*/
          (exploredNodesInSCC, nodesCounter)
        } else /*if (
            rankedNodes.nonEmpty &&
              currentArc.tail != currentArc.head
          )*/ {
          if (exploredNodesInSCC.contains(currentArc.head)) {
            /*unchanged*/
            (exploredNodesInSCC, nodesCounter)
          } else {
            /*return value*/
            (currentArc.head +: exploredNodesInSCC, nodesCounter + 1)
          }
        }
      /*!may be time consuming because of nested loop!*/
      val allArcsFromLastExplored: /*List*/ Vector[Arc] =
        graph
        .collect(
        { case a if (
          /*start with explored 'node'*/
          a.tail == currentArc.head &&
            /*where 'arc's 'head' yet unexplored*/
            !exploredNodesInSCC.contains(a.head)
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
                           /*only needed when called from 'findAllSCCwithBFS'*/
                           /*instead better return 'exploredNodesInSCC'*/
                           /*unExploredGraphNodes: Vector[Int]
                           = Vector.empty[Int],*/
                           /*used for initialization only*/
                           startingNode: Int,
                           nextArcToCheckQueue: Queue[Arc] = Queue.empty[Arc],
                           /*all corresponding graph 'nodes'*/
                           graphNodes: Array[IsExploredNode],
                           nodesCounter: Int = 0
                           //): Int = {
                           ): Int = {
    if (
      (nodesCounter > 0 ) &&
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
          graphNodes(currentArc.head - 1).isExplored = true
          /*return value*/
          nodesCounter + 1
        } else if (
                 (nodesCounter > 0 ) &&
                   currentArc.tail == currentArc.head
               ) {
          /*self loops skip*/
          /*unchanged*/
          nodesCounter
        } else /*if (
            rankedNodes.nonEmpty &&
              currentArc.tail != currentArc.head
          )*/ {
          /*ont-to-one correspondence between 'node'.value & index*/
          if (graphNodes(currentArc.head - 1).isExplored) {
            /*unchanged*/
            nodesCounter
          } else {
            /*side effect*/
            graphNodes(currentArc.head - 1).isExplored = true
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
          a.tail == currentArc.head &&
            /*where 'arc's 'head' yet unexplored*/
            !graphNodes(a.head - 1).isExplored
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
                         /*may be worth it to be shrinked in each iteration*/
                         graph: Vector[Arc],
                         /*all corresponding graph 'nodes'*/
                         graphNodes: Array[IsExploredNode],
                               /*as 'nodes' are range from '1' to 'nodesLimit'*/
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
        val         exploredConnectedComponent: Int =
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
