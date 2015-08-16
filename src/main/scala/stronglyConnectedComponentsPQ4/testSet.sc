import scala.collection.BitSet
val fruit = Set("apple", "orange", "peach", "banana")
fruit("peach")
fruit contains "peach"
fruit contains "potato"
fruit("potato")
Set("orange", "peach") subsetOf fruit
Set("orange", "cucumber") subsetOf fruit
/*not ?preserve order?*/
fruit.head
fruit.tail
fruit.last
val digitSeq1 = Set(1,2,3)
val digitSeq2 = digitSeq1 + (1,2,3)
val digitSeq3 = digitSeq1 + (1,2,3,4)
val digitSeq4 = digitSeq3 + (8,7,6,5)
val digitSeq5 =
  (9 to 0 by -1).toSet
val digitSeq6 = Set(9, 8, 7, 6, 5, 4, 3, 2, 1, 0)
val digitSeq7 = Set(9, 8, 7, 6, 5, 4, 0)
//.map(digitSeq1 + _)
//.flatten(digitSeq1 + _)
//.flatMap(digitSeq1 + _)
//.fold[Set[Int] >: Int](digitSeq1)(_ + _)
//Bitsets are sets of non-negative integers
/*?has one inner sorting order - increasing?*/
val bitSet1: BitSet = BitSet.empty
val bitSet2: BitSet = bitSet1 + 1
val bitSet3: BitSet = bitSet2 + (2)
val bitSet4: BitSet = BitSet(3, 4, 5)
val bitSet5: BitSet = bitSet3 | bitSet4
val bitSet6: BitSet = BitSet(3, 5, 7)
val bitSet7: BitSet = bitSet5 | bitSet6
val bitSet8: BitSet =
  BitSet(9, 8, 7, 6, 5, 4, 3, 2, 1, 0)
9 to 0 by -1
bitSet7(0)
bitSet7.contains(0)
bitSet7.contains(7)
bitSet7 - (7)
bitSet7.head
bitSet7.tail
bitSet7.last
bitSet7.drop(1)
bitSet7.init
bitSet8.head
bitSet8.tail
bitSet8.last
bitSet8.drop(1)
bitSet8.init
digitSeq1.subsetOf(bitSet8)
digitSeq1.subsetOf(bitSet6)
bitSet6.subsetOf(bitSet8)
digitSeq1.diff(bitSet6)
bitSet6.diff(digitSeq1)
val notInDigitSeq1 =
  digitSeq7.diff(digitSeq1)
//digitSeq1.subsetOf(digitSeq4)
//digitSeq1.subsetOf(digitSeq7)
val notInDigitSeq7 =
digitSeq1.diff(digitSeq7)
Set.empty[Int].diff(Set.empty[Int])
Set.empty[Int].diff(digitSeq7)
digitSeq7.diff(Set.empty[Int])
