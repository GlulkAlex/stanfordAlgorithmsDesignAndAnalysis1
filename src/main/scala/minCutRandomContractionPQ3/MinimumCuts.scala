package minCutRandomContractionPQ3

import scala.math.log
import randomGenerators.RandomGenerators.randomIntWithinInterval

/**
 * Created by gluk-alex on 7/23/15.
 */
object MinimumCuts {

  /*
  Programming Question - 3
  Download the text file 'kargerMinCut.txt'.
  (Karger's algorithm is
  a randomized algorithm to
  compute a `minimum cut` of
  a `connected graph`.
  It was invented by David Karger)
  The file contains
  the `adjacency list` representation of
  a simple undirected `graph`.
  There are
  '200' `vertices` labeled '1 to 200'.
  The first column in the file represents
  the `vertex` label, and
  the particular `row` (other entries except the first column)
  tells
  all the `vertices` that
  the `vertex` is adjacent to.
  So for example,
  the '6-th' row looks like :
  "6	155	56	52	120	......".
  This just means that
  the vertex with label '6' is adjacent to
  (i.e., shares an `edge` with)
  the vertices with `labels` '155,56,52,120,......,etc'
  DONE
  Your task is
  to code up and run
  the `randomized contraction algorithm`
  for the `min cut` problem and
  use it on
  the above graph to
  compute the `min cut`
  (i.e., the minimum-possible number of `crossing edges`).
  (HINT:
  Note that
  you'll have to
  figure out
  an implementation of `edge contractions`.
  Initially,
  you might want to
  do this naively,
  creating a new `graph` from the old
  every time there's an `edge` contraction.
  But
  you should also
  think about
  more efficient implementations.)
  (WARNING:
  As per the video lectures,
  please make sure
  to run the algorithm many times
  with different `random seeds`, and
  remember the `smallest cut` that you ever find.)
  Write your numeric answer in the space provided.
  So e.g., if your answer is '5',
  just type '5' in the space provided.
   */

  case class Edge(
                   startNode: Int,
                   endNode: Int)

  case class VvsE(
                   v: Int,
                   /*list of 'v'*/
                   edges: List[Int])

  def trailsNumber(nVertices: Int): Int = {
    /*vertices*/
    /*return value*/
    (nVertices * nVertices * log(nVertices)).toInt
  }

  /*switch from 'adjacencyList' to separate 'nodes' & 'edges'*/
  @scala.annotation.tailrec
  def extractGraphVandE(
                         adjacencyList: Vector[String],
                         extractedComponents: Vector[VvsE] =
                         Vector.empty[VvsE]
                         ): Vector[VvsE] = {
    if (adjacencyList.isEmpty) {
      /*return value*/
      //Vector.empty[VvsE]
      extractedComponents
    } else {
      val stringSplit: Array[Int] =
        adjacencyList
        .head
        .split(" ")
        .map(_.toInt)
      /*recursion*/
      extractGraphVandE(
                         adjacencyList.tail,
                         /*
                         ? order does not matter because of random node
                         merge ?
                          */
                         VvsE(
                               stringSplit
                               .head,
                               stringSplit
                               .tail
                               .toList) +: extractedComponents
                       )
    }
  }

  /*switch from 'adjacencyList' to separate 'nodes' & 'edges'*/
  @scala.annotation.tailrec
  def extractGraphComponents(
                              adjacencyList: Iterator[String],
                              extractedComponents: Vector[VvsE] =
                              Vector.empty[VvsE]
                              ): Vector[VvsE] = {
    if (adjacencyList.isEmpty) {
      /*return value*/
      extractedComponents
    } else /*if (adjacencyList.hasNext)*/ {
      val stringSplit: Array[Int] =
        adjacencyList
        .next()
        //CHARACTER TABULATION
        //.split(" ")
        .split('\u0009')
        .map(_.toInt)
      /*recursion*/
      extractGraphComponents(
                              /*reduced already by '.next()'*/
                              adjacencyList,
                              /*? order does not matter because of random
                              node merge ?*/
                              VvsE(
                                    stringSplit
                                    .head,
                                    stringSplit
                                    .tail
                                    .toList) +: extractedComponents
                            )
    }
  }

  /*switch from 'adjacencyList' to separate 'nodes' & 'edges'*/
  @scala.annotation.tailrec
  def extractEdges(
                    adjacencyList: Iterator[String],
                    edges: Vector[Edge] =
                    Vector.empty[Edge],
                    nodes: Vector[Int] =
                    Vector.empty[Int]
                    ): (Vector[Edge], Vector[Int]) = {
    if (adjacencyList.isEmpty) {
      /*return value*/
      (edges, nodes)
    } else /*if (adjacencyList.hasNext)*/ {
      val stringSplit: Array[Int] =
        adjacencyList
        .next()
        //CHARACTER TABULATION
        .split('\u0009')
        .map(_.toInt)
      val currentStartNode: Int =
        stringSplit
        .head
      val endNodeCandidates: Array[Int] =
        stringSplit
        .tail
      /*inner loop*/
      val newEdges: Vector[Edge] =
        (for {
          node <- endNodeCandidates
          if !edges.contains(
                              Edge(
                                    node,
                                    currentStartNode))
        } yield Edge(
                      currentStartNode,
                      node
                    )).toVector
      /*recursion*/
      extractEdges(
                    /*reduced already by '.next()'*/
                    adjacencyList,
                    /*? order does not matter because of random node merge ?*/
                    edges.union(newEdges),
                    currentStartNode +: nodes
                  )
    }
  }

  /*return cut with edges (number) connected two merged / fused sub graphs*/
  /*
  random edge from edges
  as removed
  then
  replace all occurrences of
  'endNode' with 'startNode'
  remove / discard edge with both node = 'startNode'
  flush repeat
  until
  only two nodes remains
  so
  must keep track changes in nodes list
  return value
  size / length of the final 'edges'
  */
  def randomizedEdgeContraction(
                                 //graphToCut: Vector[VvsE]
                                 nodesRemains: Int,
                                 remainingEdges: Vector[Edge]
                                 ): Int = {
    assume(nodesRemains >= 2, s"graph must have at least two nodes")
    assume(remainingEdges.nonEmpty, s"graph must have at least one edge")

    if (nodesRemains == 2) {
      /*return value*/
      remainingEdges.length
    } else {
      val loBound: Int =
        0
      val hiBound: Int =
        remainingEdges.length - 1
      val replaceEdgeIndex: Int =
        randomIntWithinInterval(loBound, hiBound)
      val replaceEdge: Edge =
        remainingEdges(replaceEdgeIndex)
      val startNode: Int = replaceEdge.startNode
      val endNode: Int = replaceEdge.endNode
      /*
      In graph theory,
      an `edge contraction` is
      an operation
      which `removes` an `edge` from a `graph`
      while
      simultaneously
      `merging`
      the two `vertices` that it previously joined.
       */
      /*node replacement after / as fusion*/
      val newRemainingEdges: Vector[Edge] =
        for {
          edge <- remainingEdges
          /*must remove cycles to merged nodes*/
          //if edge.startNode != startNode && edge.endNode != endNode
          if !(edge == replaceEdge ||
            /*reversed 'replaceEdge'*/
            (edge.startNode == endNode && edge.endNode == startNode) ||
            /*just to be sure*/
            /*as test showed those conditions matters */
            (edge.startNode == startNode && edge.endNode == startNode) ||
            (edge.startNode == endNode && edge.endNode == endNode)
            )
        } yield Edge(
                      startNode =
                        if (edge.startNode == endNode) {
                          startNode
                        /*}else if (edge.startNode == startNode) {
                          startNode*/
                        } else {
                          edge.startNode
                        },
                      endNode =
                        if (edge.endNode == endNode) {
                          startNode
                          /*}else if (edge.startNode == startNode) {
                          //? that case must be filtered out by 'for' `guard` ?
                          startNode*/
                        } else {
                          edge.endNode
                        }
                    )
      /*recursion*/
      randomizedEdgeContraction(
                                 /*reduction to converge to base case*/
                                 /*'endNode' gone*/
                                 nodesRemains - 1,
                                 remainingEdges = newRemainingEdges
                               )
    }
    /*
    randomly select from:
    > all remaining `edges`
    then pick one end node ?
    or
    > choose non merged `node` & `fuse` her `edges` ?
     */
  }

  def minimumCutTrails(
                        adjacencyList: Iterator[String] ,
                        //smallestCut: Int = Double.PositiveInfinity
                        //.toInt
                        trailsUpperBound: Int = 0
                        ) = {
    assume(adjacencyList.nonEmpty,s"'adjacencyList' shouid be `nonEmpty`")
    /*val graphComponents: Vector[VvsE] =
      extractGraphVandE(adjacencyList: Vector[String])*/
    val (graphEdges, graphNodes): (Vector[Edge], Vector[Int]) =
      extractEdges(adjacencyList)
    val trails: Int =
      if (trailsUpperBound > 0) {
        trailsUpperBound
      } else {
        trailsNumber(
                      nVertices =
                        /*fails as iterator empty at this moment*/
                        //adjacencyList.length
                        graphNodes.length
                    )
      }

    /*return 'smallestCut'*/
    def loop(
              trailsLeft: Int,
              smallestCut: Int
              ): Int = {
      //if (trailsLeft == 0) {
      if (trailsLeft <= 0) {
        /*return value*/
        smallestCut
      } else /*if (trailsLeft > 0)*/{
        /*may changes over time or stay the same depending on graph*/
        val currentCut: Int =
          randomizedEdgeContraction(
                                     nodesRemains =
                                       graphNodes.length,
                                     remainingEdges =
                                       graphEdges
                                   )
        /*recursion*/
        loop(
              trailsLeft - 1,
        /*must reduce with every iteration,
        at least in the first turn,
        not stay the same*/
              smallestCut =
                if (smallestCut > currentCut) {
                  currentCut
                } else {
                  smallestCut
                }
            )
      }
    }

    /*initialization*/
    loop(
          trailsLeft = trails,
          smallestCut =
            Double.PositiveInfinity
            .toInt
        )
  }
}
