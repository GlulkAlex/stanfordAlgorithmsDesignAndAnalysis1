val ranD1 = scala.math.random
val ranD2 = scala.math.random

ranD1.signum
ranD1.isWhole()
ranD1.floor
ranD1.ceil
ranD1.round
ranD1.toString.length
"2147483647".length
"0"*9
val intShift1: Int = 1000000000
val intShift2: Int = 100000000
(ranD1 * intShift2).toInt
val len3_7 = 7-3 + 1
/*from 0 to 1 / length => 0.2*/
/*3=>0.0<=r3<0.2 => * 10 = 0 or 1 */
/*4=>0.2<=r4<0.4 => * 10 = 2 or 3 */
/*5=>0.4<=r5<0.6 => * 10 = 4 or 5 */
/*6=>0.6<=r5<0.8 => * 10 = 6 or 7 */
/*slightly less then previous*/
/*not even possibility*/
/*7=>0.8<=r5<1 => * 10 = 8 or 9 */
val threshold: Double = 1.0/len3_7
val remainder: Double = ranD1 % ranD1
val ratio: Double =
  //ranD1 / len3_7
  len3_7 / ranD1

lazy val streamOfPossibleVals: Stream[Double] =
  (for {i <- 0 to 10} yield {
    i * threshold
  }).toStream

(ranD1 / threshold).floor
(ranD1 / threshold).toInt
(ranD2 / threshold).floor
(ranD2 / threshold).toInt