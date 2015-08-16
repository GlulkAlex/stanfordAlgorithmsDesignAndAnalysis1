import scala.annotation.tailrec
import scala.util.Try

/*
The following code
solves
a few `DFS` related `graph` problems on `directed graphs`.
It is not the most elegant code, but
if I am not mistaken it is:
>Tail recursive.
>Uses only `immutable` collections (and `iterators` on them).
>Has optimal time O(|V| + |E|) and space complexity (O(|V|).
 */
/**
 * Created with IntelliJ IDEA.
 * User: mishaelr (Mishael Rosenthal)
 * Date: 5/14/14
 * Time: 5:18 PM
 */
object DirectedGraphTraversals {

  type Graph[Vertex] = Map[Vertex, Set[Vertex]]

  def dfs[Vertex](
                   graph: Graph[Vertex],
                   initialVertex: Vertex) =
    dfsRec(DfsNeighbours)(
                           graph,
                           List(
                                 DfsNeighbours(
                                                graph,
                                                initialVertex,
                                                Set(),
                                                Set())),
                           Set(),
                           Set(),
                           List())

  def topologicalSort[Vertex](graph: Graph[Vertex]) =
    graphDfsRec(TopologicalSortNeighbours)(
                                            graph,
                                            graph.keySet,
                                            Set(),
                                            Set(),
                                            List())

  def stronglyConnectedComponents[Vertex](graph: Graph[Vertex]) = {
    val exitOrder =
      graphDfsRec(DfsNeighbours)(
                                  graph,
                                  graph.keySet,
                                  Set(),
                                  Set(),
                                  List())
    val reversedGraph = reverse(graph)

    exitOrder
    .foldLeft(
        (Set[Vertex](), List(Set[Vertex]()))
             ) {
                 case (acc@(visitedAcc, connectedComponentsAcc), vertex) =>
                   if (visitedAcc(vertex)) {
                     acc
                   }
                   else {
                     val connectedComponent: Set[Vertex] =
                       dfsRec(DfsNeighbours)(
                                              reversedGraph,
                                              List(
                                                    DfsNeighbours(
                                                                   reversedGraph,
                                                                   vertex,
                                                                   visitedAcc,
                                                                   visitedAcc)),
                                              visitedAcc,
                                              visitedAcc,
                                              List())
                       .toSet
                     (visitedAcc ++ connectedComponent,
                       connectedComponent ::
                         connectedComponentsAcc)
                   }
               }._2
  }

  def reverse[Vertex](graph: Graph[Vertex]) = {
    val reverseList: List[(Vertex, Vertex)] =
      for {
        (vertex, neighbours) <- graph.toList
        neighbour <- neighbours
      } yield (neighbour, vertex)
    /*return value*/
    reverseList
    .groupBy(_._1)
    .mapValues(
        _
        .map(_._2)
        .toSet)
  }

  private sealed trait NeighboursFunc {
    def apply[Vertex](
                       graph: Graph[Vertex],
                       vertex: Vertex,
                       entered: Set[Vertex],
                       exited: Set[Vertex]): (Vertex, Iterator[Vertex])
  }

  private object DfsNeighbours extends NeighboursFunc {
    def apply[Vertex](
                       graph: Graph[Vertex],
                       vertex: Vertex,
                       entered: Set[Vertex],
                       exited: Set[Vertex]): (Vertex, Iterator[Vertex]) =
      (vertex,
        graph
        .getOrElse(
            vertex,
            Set())
        .iterator)
  }

  private object TopologicalSortNeighbours extends NeighboursFunc {
    def apply[Vertex](
                       graph: Graph[Vertex],
                       vertex: Vertex,
                       entered: Set[Vertex],
                       exited: Set[Vertex]): (Vertex, Iterator[Vertex]) = {
      val neighbours: Set[Vertex] =
        graph
        .getOrElse(vertex, Set())

      if (neighbours
          .exists(neighbour =>
                    entered(neighbour) && !exited(neighbour))) {
        throw new IllegalArgumentException(
                                            "The graph is not a DAG, it " +
                                              "contains cycles:\n" + graph)
      }
      else {
        (vertex, neighbours.iterator)
      }
    }
  }

  @tailrec
  private def dfsRec[Vertex](neighboursFunc: NeighboursFunc)(
    graph: Graph[Vertex],
    toVisit: List[(Vertex, Iterator[Vertex])],
    entered: Set[Vertex],
    exited: Set[Vertex],
    exitStack: List[Vertex]): List[Vertex] = {
    toVisit match {
      case List()                            => exitStack
      case (currentVertex, neighbours) :: tl =>
        val filtered =
          neighbours
          .filterNot(entered)

        if (filtered.hasNext) {
          val nextNeighbour =
            filtered
            .next()

          dfsRec(neighboursFunc)(
                                  graph,
                                  neighboursFunc(
                                                  graph,
                                                  nextNeighbour,
                                                  entered,
                                                  exited) ::
                                    toVisit,
                                  entered + nextNeighbour,
                                  exited,
                                  exitStack)
        } else {
          dfsRec(neighboursFunc)(
                                  graph,
                                  tl,
                                  entered,
                                  exited +
                                    currentVertex,
                                  currentVertex :: exitStack)
        }
    }
  }

  @tailrec
  private def graphDfsRec[Vertex](neighboursFunc: NeighboursFunc)(
    graph: Graph[Vertex],
    notVisited: Set[Vertex],
    entered: Set[Vertex],
    exited: Set[Vertex],
    order: List[Vertex]): List[Vertex] = {
    if (notVisited.isEmpty) {
      order
    }
    else {
      val orderSuffix =
        dfsRec(neighboursFunc)(
                                graph,
                                List(neighboursFunc(
                                                     graph,
                                                     notVisited
                                                     .head,
                                                     entered,
                                                     exited)),
                                entered,
                                exited,
                                List())

      graphDfsRec(neighboursFunc)(
                                   graph,
                                   notVisited -- orderSuffix,
                                   entered ++ orderSuffix,
                                   exited ++ orderSuffix,
                                   orderSuffix ::: order)
    }
  }
}

/*unit test*/
object DirectedGraphTraversalsExamples extends App {

  import DirectedGraphTraversals._

  val graph = Map(
                   "B" -> Set("D", "C"),
                   "A" -> Set("B", "D"),
                   "D" -> Set("E"),
                   "E" -> Set("C"))

  println("`dfs` from 'A':" + dfs(graph, "A"))
  println("`dfs` from 'B':" + dfs(graph, "B"))

  println("'topologicalSort':" + topologicalSort(graph))

  println("initial 'graph':" + graph.mkString(","))
  println("reverse(d) 'graph':" + reverse(graph).mkString(","))
  println("'stronglyConnectedComponents' in 'graph':" +
            stronglyConnectedComponents(graph))

  val graph2 = graph + ("C" -> Set("D"))

  println("'graph2':" + graph2.mkString(","))
  println("'stronglyConnectedComponents' in 'graph2':" +
            stronglyConnectedComponents(graph2))
  println("'topologicalSort' on 'graph2':" +
            Try(topologicalSort(graph2)))
}
