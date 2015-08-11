val map1 = Map(1->"one")
val map2 = map1 + (1->"1")
val map3 = map2 + (1->"1", 2->"2", 3->"3")
val mapValIter3 = map3.valuesIterator
val mapKeyIter3 =  map3.keysIterator
mapKeyIter3.next
mapKeyIter3.next
mapKeyIter3.next