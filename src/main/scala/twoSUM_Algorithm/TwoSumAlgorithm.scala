package twoSUM_Algorithm

/**
 * Created by gluk-alex on 8/29/15.
 */
object TwoSumAlgorithm {
  /*
  The goal of this problem is to
  implement a variant of the `2-SUM algorithm`
  (covered in the Week 6 lecture on `hash table` applications).
  The file contains
  '1' million integers,
  both positive and negative
  (there might be some repetitions!).
  This is your array of integers, with
  the 'i-th' `row` of the file specifying
  the 'i-th' entry of the array.
  TODO
  Your task is to
  compute the number (collection size) of
  `target` `values` 't' in the `interval`
  [-10000,10000] (inclusive) such that
  there are `distinct` numbers 'x','y' in the input file
  that satisfy 'x + y = t' .
  (NOTE:
  ensuring `distinctness` requires
  a one-line addition to the algorithm from lecture.
  )

  Write your numeric answer
  (an integer between '0' and '20001') in the space provided.

  OPTIONAL CHALLENGE:
  If this problem is too easy for you,
  try implementing
  your own `hash table` for it.
  For example,
  you could
  compare performance under
  the chaining and
  open addressing approaches to resolving collisions.
   */

  val inputSample: String = "68037543430\n-21123414637\n56619844751" +
    "\n59688006695\n82329471587\n-60489726142\n-32955448858\n53645918962\n" +
    "-44445057840\n10793991159"

  /*
  Steps:
  >insert (Long) numbers in Set[Long]
  >>for each 'x' number in Set lookup in Set for (t - x)
  condition (guard) 'x != y'
  also may be only `distinct` t = x + y allowed
   */
  @scala.annotation.tailrec
  def collectSumWithinInterval(
                                /*for lookup*/
                                sourceSet:
                                /*scala.collection.immutable.
                                TreeSet[Long],*/
                                Set[Long],
                                /*for iteration use iterator
                                not collection copy */
                                remainsToCheckSet:
                                //Set[Long],
                                Iterator[Long],
                                result:
                                /*Stream[Long] =
                                Stream.empty*/
                                /*Set[Long] =
                                Set.empty*/
                                  scala.collection.mutable.Set[Long] =
                                scala.collection.mutable.
                                Set.empty,
                                /*lower bound*/
                                minT: Long = -10000L,
                                /*upper bound*/
                                maxT: Long = 10000L
                                ):
  /*or some sequence or data structure with fast '.add' & .size' / '.length'*/
  //Stream[Long] = {
  scala.collection.mutable.
  Set[Long] = {
    //inner loop
    @scala.annotation.tailrec
    def collectSumWithDistinctY(
                                 x: Long,
                                 t: /*Int*/ Long /*=
                                 minT*/
                                 /*-10000L*//*,
                                 innerResult:
                                 //Stream[Long]
                                 Set[Long]*/
                                 ):
    scala.collection.mutable.
    Set[Long] = {
                                 //): Stream[Long] = {
      if (t > maxT/*10000L*/) {
        /*return value*/
        //innerResult
        result
      } else {
        val possibleY: Long = t - x
        val innerResultUpdated:
        scala.collection.mutable.
        Set[Long] =
        //Stream[Long] =
          if (possibleY == x) {
            /*skip not `distinct`*/
            //innerResult
            result
          } else {
            if (
              sourceSet.contains(possibleY)
            ) {
              //x + y
              //t +: innerResult
              //innerResult + t
              result += t
            } else {
              //innerResult
              result
            }
          }
        /*recursion*/
        collectSumWithDistinctY(
                                 x = x,
                                 t = t + 1/*,
                                 innerResult = innerResultUpdated*/
                               )
      }
    }

    if (
      sourceSet.isEmpty ||
        remainsToCheckSet.isEmpty
    ) {
      /*return value*/
      result
    } else {
      /*val resultUpdated:
      scala.collection.mutable.
      Set[Long] =*/
      //Stream[Long] =
      /*side effect*/
        collectSumWithDistinctY(
                                 x =
                                   remainsToCheckSet
                                   /*to converge to empty eventually*/
                                   .next(),
                                 t = minT
                                   //.head,
                                 //innerResult = result
                               )
      /*recursion*/
      collectSumWithinInterval(
                                sourceSet = sourceSet,
                                /*converge to empty eventually*/
                                remainsToCheckSet =
                                  remainsToCheckSet,
                                  //.tail,
                                result =
                                  //resultUpdated
                                    result,
                                minT = minT,
                                maxT = maxT
                              )
    }
  }

}
