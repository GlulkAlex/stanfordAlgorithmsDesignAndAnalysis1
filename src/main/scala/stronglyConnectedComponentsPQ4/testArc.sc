//import stronglyConnectedComponentsPQ4._
import stronglyConnectedComponentsPQ4.stronglyConnectedComponents.IsExploredNode
import stronglyConnectedComponentsPQ4.stronglyConnectedComponents.Arc
//import stronglyConnectedComponentsPQ4.stronglyConnectedComponents.ArcFromNodes
//import stronglyConnectedComponentsPQ4.stronglyConnectedComponents._
case class ArcFromNodes(
                         tail: IsExploredNode,
                         head: IsExploredNode){
  override def toString =
    s"""{${tail.node}[${if (tail.isExplored) "e" else "u"}]->"""+
      s"""${head.node}[${if (head.isExplored) "e" else "u"}]}"""
}
val node1 =
  IsExploredNode(1,false)
val node2 =
  IsExploredNode(2,false)
val node3 =
  IsExploredNode(3,false)
val nodes =
  Vector(node1,node2,node3)
val arc1_2: ArcFromNodes =
/*stronglyConnectedComponentsPQ4
.stronglyConnectedComponents
.*/ArcFromNodes(node1, node2)
val arc1_3: ArcFromNodes =
ArcFromNodes(node1, node3)
val arc2_3: ArcFromNodes =
ArcFromNodes(node2, node3)
node2.isExplored = true
arc1_2
arc1_3
arc2_3
arc1_2.tail.isExplored = true
arc1_2
arc1_3
arc2_3
val arcSimple1_2 = Arc(1,2)
arcSimple1_2.arcTail - 1
nodes(arcSimple1_2.arcTail - 1)