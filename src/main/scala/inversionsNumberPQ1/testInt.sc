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