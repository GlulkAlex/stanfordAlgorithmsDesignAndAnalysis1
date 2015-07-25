package randomGenerators

import java.util.Random
import scala.math.random
import scala.math.abs
import scala.math.BigDecimal.RoundingMode

/**
 * Created by gluk-alex on 7/25/15.
 */
object RandomGenerators {

  trait Generator[+T] {
    self =>
    /*abstract*/
    def generate: T

    def map[S](f: T => S): Generator[S] =
      new Generator[S] {
        def generate = f(self.generate)
      }

    def flatMap[S](f: T => Generator[S]): Generator[S] =
      new Generator[S] {
        def generate = f(self.generate).generate
      }
  }

  /*a basic integer (positive & negative) random generator*/
  val integers = new Generator[Int] {
    /*
    `IntStream`	ints(long streamSize, int randomNumberOrigin, int
    randomNumberBound)
    Returns
    a stream
    producing
    the given 'streamSize' number of pseudorandom int values,
    each conforming to the given `origin` (inclusive) and `bound` (exclusive).
    */
    val rand = new java.util.Random

    /*
    All 2^32 possible int values are produced
    with (approximately) equal probability.
    */
    //public int nextInt(int bound)
    /*
    Parameters:
      bound - the upper bound (exclusive). Must be positive.
    Returns:
      the next pseudorandom, uniformly distributed int value
      between zero (inclusive) and
      bound (exclusive) from this random number generator's sequence
     */
    /*actual concrete implementation*/
    def generate = rand.nextInt()
  }

  /*a basic doubles random generator*/
  val doubles = new Generator[Double] {
    /*
    Returns
    a double value
    with a positive sign,
    greater than or equal to 0.0 and less than 1.0
    */
    //val rand: Double = random

    /*actual concrete implementation*/
    def generate = random
  }

  val booleans: RandomGenerators.Generator[Boolean] =
    for {x <- integers} yield x > 0
  val pairs: RandomGenerators.Generator[(Int, Int)] =
    for {
      x <- integers
      y <- integers
    } yield (x, y)

  /*? is return value always positive ? - No*/
  def interval(
                lo: Int,
                hi: Int): Generator[Int] =
    for {x <- integers} yield lo + x % (hi - lo)

  /*check for low bound sign*/
  def intervalImproved(
                        lo: Int,
                        hi: Int): Generator[Int] =
    for {x <- integers} yield
    if (lo >= 0) {
      lo + abs(x) % (hi - lo)
    } else {
      lo + x % (hi - lo)
    }

  def numericValWithPrecision(
                     //numericVal: BigDecimal,
                     numericVal: Double,
                     /*must be positive*/
                     decimalPartLength: Int = 2
                     ): BigDecimal = {
    //BigDecimal class to handle money:
    //val salary = 100000: BigDecimal
    //salary: BigDecimal = 100000
    //val weekly = salary / 52
    //weekly: scala.math.BigDecimal = 1923.076923076923076923076923076923
    //weekly.setScale(2, RoundingMode.HALF_EVEN)
    //res0: scala.math.BigDecimal = 1923.08
    //BigDecimal.decimal(0.1)
    /*return value*/
    //numericVal
    BigDecimal
    .decimal(numericVal)
    .setScale(
        decimalPartLength,
        RoundingMode.HALF_EVEN)
  }
  
  def truncateDoubleValTo(
                           numericVal: Double,
                           /*must be positive*/
                           decimalPartLength: Int = 2
                           ): Double = {
    assume(
            decimalPartLength >= 0,
            s"'decimalPartLength' must be positive"
          )
    def evalPrecision(
                       lengthRange: Range,
                       currentPrecision: Double = 1
                       ): Double = {
      if (lengthRange.isEmpty) {
        /*return value*/
        currentPrecision
      } else {
        evalPrecision(
                       lengthRange.tail,
                       currentPrecision*0.1
                     )
      }
    }
    //Accuracy:
    val precision: Double =
      //(for (i<- 0 to decimalPartLength) yield i * 0.1).min
      //(0 to decimalPartLength).reduce(_ * _)
      scala.math.pow(0.1, decimalPartLength)
    /*return value*/
    numericVal -
      numericVal % precision -
      /*extra*/
      numericVal % 0.00000000000000001
  }

  /*'0.16000000000000003' still possible*/
  def doublesTruncated(accuracy: Int) = new Generator[Double] {
    /*actual concrete implementation*/
    def generate = truncateDoubleValTo(
                                        doubles.generate,
                                        accuracy
    )
  }
}
