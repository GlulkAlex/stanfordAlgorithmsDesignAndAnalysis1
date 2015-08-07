import stronglyConnectedComponentsPQ4.stronglyConnectedComponents.Arc

case class RankedNode(node: Int, rank: Int)

val ranked = List(
                   RankedNode(3,1),
                     RankedNode(2,1),
                     RankedNode(1,0)
                 )

val rankedUnziped: (List[Int], List[Int]) =
ranked.unzip(r=>(r.node,r.rank))

val pickRanked1 =
  ranked
.collect(
{case r if (
  r.node == 3
  ) => r}
        )
val mockUpGraph: Vector[Arc] =
  Vector(
          Arc(1, 2),
          Arc(1, 3),
          Arc(2, 5),
          Arc(3, 5),
          Arc(3, 4),
          Arc(4, 5),
          Arc(4, 6),
          Arc(5, 6)
        )
val pickRanked2 =
  mockUpGraph
  .collect(
  {case a if (
    a.tail == 3 &&
    //!ranked.contains(RankedNode(a.head,_: Int)) //&&
    //!ranked.contains(RankedNode(a.tail,_: Int))
    //ranked.contains(RankedNode(a.tail,_: Int))
    //ranked.exists(_.node == a.tail)
    //!ranked.exists(_.node == a.tail)
    !ranked.exists(_.node == a.head)
    ) => a}
          )
mockUpGraph.length
pickRanked2.length
