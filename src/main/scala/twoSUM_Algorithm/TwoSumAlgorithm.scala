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
  the 'ith' `row` of the file specifying
  the 'ith' entry of the array.
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
}
