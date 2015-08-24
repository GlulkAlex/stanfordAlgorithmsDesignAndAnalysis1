val map1 = Map(1->"one")
val map2 = map1 + (1->"1")
val map3 = map2 + (1->"1", 2->"2", 3->"3")
val mapValIter3 = map3.valuesIterator
val mapKeyIter3 =  map3.keysIterator
mapKeyIter3.next
mapKeyIter3.next
mapKeyIter3.next
map1.head._1
map1.head.x
map1.head.swap
val (key,vaLue) =
map1.head
var varMap4: Map[Int, String] =
  map2 + (1->"1", 2->"2", 3->"3")
val mapElemWithKey1 = varMap4.get(1).get
varMap4.updated(1,"one")
varMap4
varMap4 =
varMap4.updated(1,"one")
varMap4
mapElemWithKey1
for {
  (nodeKey, nodeVal) <- map3
  if nodeVal == "2"
} yield nodeKey
map3
.view
.take(15)
.map(
//(k,v): (Int,NodeMapValFieldsStatic) =>
{ case (k, v) =>
  k + "" +
    v
    //.adjustedNodes
    .mkString("{", ",", "}")
}
    )
//.toIterable
.mkString(",")
map3
.view
.take(15)
.collect(
{ case (k, v) if (v.toInt % 2) != 0 =>
    v
}
    )
.toSet