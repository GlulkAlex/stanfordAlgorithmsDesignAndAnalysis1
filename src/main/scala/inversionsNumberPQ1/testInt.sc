-1-1
-1-1 < -1
-1-1 < -2
val len0 = 0
val len1 = 1
val len2 = 2
val len3 = 3
val len4 = 4
val len5 = 5
val len6 = 6
val len7 = 7

def partition(len: Int): (Int,Int) = {
  val fP: Int = firstPart(len)

  (fP, secondPart(fP, len))
}

def firstPart(len: Int): Int = {
  if (
    len <= 0) {
    0
  /*} else if (
             len == 1 ||
             len/2 == 1
           ) {
    1*/
  } else /*if (len/2 > 1)*/ {
    len/2 + len%2
  }
}

def secondPart(fPart: Int, len: Int): Int = {
  if (fPart <= 0) {
    0
  } else if (len - fPart > 0) {
    len - fPart
  } else /*if (fPart > 0)*/ {
    0
  }
}

len0/2
partition(len0)
len1/2
partition(len1)
len2/2
partition(len2)
len3/2
len3/2 + len3%2
partition(len3)
len4/2
partition(len4)
len5/2
partition(len5)
partition(len6)
partition(len7)

0 - 0
0 + 1 - 0
3>2
//0,1,2
(3-0)/2
//0,1,2,3,4
(4-0)/2
4>2
4%2
//0,1,2,3
(3-0)/2
//0,1,2,3,4,5
(5-0)/2
//0,(1,[2],3,4),5
(1) + (4-1)/2
//0,1,(2,3,[4],5,6),7
(2) + (6-2)/2
//([0])
(0) + (0-0)/2
//([0],1)
(0) + (1-0)/2
//0,1,2,3,([4],5),6,7
(4) + (5-4)/2
val len3_7 = 7-3 + 1
/*from 0 to 1 / length => 0.2*/
/*3=>0.0<=r3<0.2 => * 10 = 0 or 1 */
/*4=>0.2<=r4<0.4 => * 10 = 2 or 3 */
/*5=>0.4<=r5<0.6 => * 10 = 4 or 5 */
/*6=>0.6<=r5<0.8 => * 10 = 6 or 7 */
/*slightly less then previous*/
/*not even possibility*/
/*7=>0.8<=r5<1 => * 10 = 8 or 9 */