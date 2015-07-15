package inversionsNumberPQ1

import scala.Predef
//import java.lang.stackoverflowerror
import java.lang.StackOverflowError

//import scala.io.Source
//import scala.Predef.intArrayOps
//import scala.io.StdIn.readInt
//import scala.io.StdIn.readLine

/**
 * Created by Alex on 02.07.15.
 */
class InversionsNumber {
  /*
  Programming Question-1
  The due date for this homework is Sun 19 Jul 2015 11:59 PM PDT.
  This 'IntegerArray.txt' file contains
  all of the 100,000 integers
  between 1 and 100,000 (inclusive)
  in some order,
  with no integer repeated.

  TODO
  Your task is
  to compute
  the `number of inversions`
  in the file given,
  where
  the i-th row of the file indicates
  the i-th entry of an array.
  Because of
  the large size of this array,
  you should implement
  the fast `divide-and-conquer` algorithm covered in the video lectures.
  The numeric answer for
  the given input file
  should be typed in the space below.
  So if your answer is '1198233847', then
  just type '1198233847' in the space provided
  without any 'space' / 'commas' / any other 'punctuation marks'.
  You can make up to 5 attempts, and
  we'll use the best one for grading.
  (We do not require you to
  submit your code, so
  feel free to
  use any programming language you want ---
  just type the final numeric answer in the following space.)

  [TIP:
  before submitting,
  first test the correctness of your program on
  some small test files or
  your own devising.
  Then
  post your best test cases to
  the discussion forums to
  help your fellow students!]
  */

  /*Int	32 bit signed value. Range -2147483648 to 2147483647*/
  ///*lazy*/ val linesInInput: /*Int*/ Long = countLines(getInput())
  //val linesInput: Iterator[String] = getInput
  val linesInInputStream: Stream[String] =
    getFileContent()
  /*val linesInInputArray: scala.Array[String] =
    linesInInputStream
      .toArray*/
  val inegersInInputStream: Stream[Int] =
    linesInInputStream
    .map(_.toInt)
  val integersInInputSeq: Seq[Int] =
  //for (i<-linesInInputStream.indices) yield Integer.getInteger
  // (integersInInputSeq(i))
    for (elem <- linesInInputStream) yield
    Integer
    .parseInt(elem)
  val integersInInputArray: Array[Int] =
    integersInInputSeq
    .toArray

  /*if path & name OK this is enough*/
  def getInput: Iterator[String] = {
    val filename = "IntegerArray.txt"
    val filePath =
      "E:\\Java\\coursera-workspace\\Stanford_algo2-004\\src\\unitTests\\"
    /*val currFile = Source
      .fromFile(filePath + filename)*/
    val currFileLines =
      scala.io.Source
      .fromFile(filePath + filename)
      .getLines()
    /*return value*/
    currFileLines
  }

  /*lazy on demand*/
  def getFileContent(
                      filename: String = "IntegerArray.txt",
                      filePath: String =
                      //"E:\\Java\\Scala\\sbt\\projects\\" +
                      //"stanfordAlgorithmsDesignAndAnalysis1\\src\\" +
                      /*"..\\src\\" +
                      "test\\scala\\testInversionsNumber\\"*/
                      //"../src/test/scala/testInversionsNumber/"
                      //"E:\\Java\\coursera-workspace\\Stanford_algo2-004
                      // \\src\\unitTests\\"
                      "/media/gluk-alex/GDI/Java/Scala/sbt/projects/" +
                        "stanfordAlgorithmsDesignAndAnalysis1/src/test/scala" +
                        "/testInversionsNumber/"
                      //"IntegerArray.txt"
                      ): Stream[String] = {
    /*return value*/
    scala.io.Source
    .fromFile(filePath + filename)
    .getLines()
    .toStream
  }

  def partition(len: Int): (Int, Int) = {
    val fP: Int = firstPart(len)

    (fP, secondPart(fP, len))
  }

  /*'firstPart' always bigger / greater then 'secondPart'*/
  def firstPart(len: Int): Int = {
    if (
      len <= 0) {
      0
    } else /*if (len/2 > 0)*/ {
      len / 2 + len % 2
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

  /*
  * all Divide&Conquere may be correspond to
  * indexes ranges -
  * hole array length divided by equal (when possible) parts / partitioning
  * Task is:
  * at any given moment of time
  * calculate right range value
  *
  * Cases:
  * >sequence empty => Done
  * >has one element => Done
  * >has two elements =>
   *  part1.size1
   *  part2.size1
   *  compare
   *  swap / switch (& shiftElemRightUntilOrdered) if needed
  *   Done
  * >after 'size1' processed -
  *   leader pointer
  *   last / some index in (part1 or 2) (exceed or equal sequence.length)
   *  if size1 + 1 >= sequence.length
    *  Done
    *  else
    *  newSize = oldSize + 1
    *  until not `index` in part1 or 2 out of bound / range
    *  `merge` elements in part1 with part2
    *  check `newSize` vs 'sequence.length'
     *  process `newSize` `merge` or
     *  Done
     *  >if part1.size < `CurrentPartSize` => Done
     *  >if part2.size == 0 => Done
  * */
  /*array content sorted as side effect (: Unit) no new collection created*/
  /*basically interesting in 'swapCount'*/
  def mergeArraySortedParts(
                             /*unchanging*/
                             sourceArray: Array[Int] = Array.emptyIntArray,
                             /*unchanging*/
                             sourceArrayLength: Int = 0,
                             /*changing*/
                             //? must change outside ?
                             //? redundant / useless ?
                             currentPartSize: Int = 0,
                             /*changing*/
                             firstPartStart: Int = -1,
                             /*changing*/
                             firstPartEnd: Int = 0,
                             /*changing*/
                             firstPartSize: Int = 0,
                             /*changing*/
                             firstPartLeader: Int = 0,
                             /*changing*/
                             secondPartStart: Int = -1,
                             /*changing*/
                             secondPartEnd: Int = 0,
                             /*changing*/
                             secondPartSize: Int = 0,
                             /*changing*/
                             //? redundant / useless ?
                             //secondPartLeader: Int = 0,
                             /*changing*/
                             swapCount: Long = 0L
                             //): Array[Int] = {
                             ): Long = {
    if (
    /*nothing to sort*/
      sourceArray.isEmpty ||
        /*already sorted*/
        sourceArrayLength == 1 ||
        /*have only already sorted first part*/
        firstPartSize < currentPartSize ||
        //(firstPartEnd - firstPartStart) < currentPartSize - 1 ||
        /*no second part*/
        secondPartSize <= 0 ||
        //(secondPartEnd - secondPartStart) == 0 ||
        /*sorting Done*/
        firstPartLeader > firstPartEnd ||
        firstPartLeader == -1
    ) {
      /*return value*/
      //sourceArray
      swapCount
    } else {
      /*actual work*/
      if (sourceArrayLength == 2) {
        /*special case*/
        if (sourceArray(0) > sourceArray(1)) {
          swapElements(
                        sourceArray: Array[Int],
                        indexOfLesser = 1,
                        indexOfGreater = 0
                      )
          /*return value*/
          swapCount + 1
        } else {
          /*return value*/
          //sourceArray
          swapCount
        }
      } else /*if (sourceArrayLength > 2)*/ {
        val part1S =
          firstPartStart
        val part1E =
          firstPartEnd
        val part2S =
          secondPartStart
        val part2E =
          secondPartEnd
        val newPartSize: Int =
          currentPartSize

        /*var newPart1Leader =
          firstPartLeader*/
        /*var newPart2Leader =
          secondPartLeader*/
        var newSwapCount: Long =
          swapCount

        /*compare 'part1.head' with / vs. 'part2.head'*/
        if (
          sourceArray(firstPartLeader) > sourceArray(
                                                      /*secondPartLeader*/
                                                      secondPartStart)
        ) {
          /*change order*/
          swapElements(
                        sourceArray: Array[Int],
                        indexOfLesser =
                          //secondPartLeader,
                          secondPartStart,
                        indexOfGreater = firstPartLeader
                      )
          /*if 'indexOfLesser' / 'secondPartStart' == 'secondPartEnd' then
          Done */
          if (secondPartStart == secondPartEnd) {
            /*Done. in part2 all sorted*/
            newSwapCount = swapCount + 1
          } else {
            /*restore order*/
            newSwapCount =
              try {
                shiftElemRightUntilOrdered(
                                            sourceArray: Array[Int],
                                            elemIndex =
                                              //secondPartLeader,
                                              secondPartStart,
                                            rangeStartIndex = secondPartStart
                                              + 1,
                                            rangeEndIndex = secondPartEnd,
                                            swapCount = swapCount + 1
                                          )
              }
              catch {
                case e: StackOverflowError =>
                  println("StackOverflowError")
                  newSwapCount
                case e: Throwable => newSwapCount
                case e: Exception => newSwapCount
              }
          }
          /*next*/
          //newPart1Leader =
          /*if available*/
          //newPart1Leader + 1
          //newPart2Leader //same
        } else {
          /*same order*/
          /*next*/
          //newPart1Leader =
          /*if available*/
          //newPart1Leader + 1
          //newPart2Leader //same
        }

        /*recursion*/
        try {
          mergeArraySortedParts(
                                 /*unchanging*/
                                 sourceArray = sourceArray,
                                 /*unchanging*/
                                 sourceArrayLength = sourceArrayLength,
                                 /*changing ?*/
                                 currentPartSize =
                                   //newPartSize,
                                   currentPartSize,
                                 /*changing ?*/
                                 firstPartStart = firstPartStart,
                                 /*changing ?*/
                                 firstPartEnd = firstPartEnd,
                                 firstPartSize = firstPartSize,
                                 /*changing*/
                                 firstPartLeader = firstPartLeader + 1,
                                 /*changing ?*/
                                 secondPartStart = secondPartStart,
                                 /*changing ?*/
                                 secondPartEnd = secondPartEnd,
                                 secondPartSize = secondPartSize,
                                 /*changing*/
                                 //? redundant / useless ?
                                 //secondPartLeader: Int = 0,
                                 /*changing*/
                                 swapCount = newSwapCount
                               )
        }
        catch {
          case e: StackOverflowError =>
            println("StackOverflowError")
            newSwapCount
          case e: Throwable => newSwapCount
          case e: Exception => newSwapCount
        }
      }
    }
    /*default*/
    //sourceArray
  }

  /*double every time*/
  def evalNewPartsSize(
                        //sourceArrayLength: Int,
                        currentPartSize: Int
                        ): Int = {
    if (currentPartSize < 1) {
      1
    } else {
      currentPartSize * 2
    }
  }

  /*grow every time*/
  /*? may be out of bound to signal Done sorting for 'currentPartSize'*/
  def evalNewFirstPartStart(
                             sourceArrayLength: Int,
                             currentFirstPartStart: Int,
                             currentPartSize: Int
                             ): Int = {
    if (
      currentFirstPartStart < 0 ||
        currentFirstPartStart >= sourceArrayLength
    ) {
      /*start new traversal cycle*/
      //? 0
      //? sourceArrayLength + 1
      currentFirstPartStart
    } else {
      /*continue current*/
      currentFirstPartStart + 2 * currentPartSize
    }
  }

  /*grow every time*/
  /*0 + 1 => 0 */
  /*end => start */
  def evalNewFirstPartEnd(
                           sourceArrayLength: Int,
                           currentFirstPartStart: Int,
                           currentPartSize: Int
                           ): Int = {
    /*may be out of bound / range*/
    if (
      currentFirstPartStart + currentPartSize >= sourceArrayLength &&
        /*has room at least for one element in 1st part*/
        currentFirstPartStart < sourceArrayLength
    ) {
      /*last available index in array*/
      sourceArrayLength - 1
    }else if (
      currentFirstPartStart + currentPartSize >= sourceArrayLength &&
        /*has no room for elements in 1st part*/
        currentFirstPartStart >= sourceArrayLength
    ) {
      /*same as start*/
      currentFirstPartStart
    } else {
      currentFirstPartStart - 1 + currentPartSize
    }
  }

  /*grow every time*/
  /*?if no room for 2nd part then '2nd part end' - '1st part end' == '0' size
   ?*/
  /*for array.length = '2' & parts.size = '3' must be same as 1st part end =
  '1' */
  /*for array.length = '3' & parts.size = '2' must be same as 2nd part start
  = '2' */
  def evalNewSecondPartEnd(
                            sourceArrayLength: Int,
                            /*? may be use 'currentFirstPartEnd' ?*/
                            currentFirstPartStart: Int,
                            currentPartSize: Int
                            ): Int = {
    /*? may be out of bound / range ?*/
    if (
    /*no room for 2nd part*/
      currentFirstPartStart + currentPartSize >= sourceArrayLength /*||
        currentFirstPartStart + currentPartSize + 1 >= sourceArrayLength*/
    ) {
      evalNewFirstPartEnd(sourceArrayLength, currentFirstPartStart,
                          currentPartSize)
    } else /*if (
           /*at least one element in 2nd part */
             currentFirstPartStart + currentPartSize < sourceArrayLength
           )*/ {
      if (
          /*room for size less then 'currentPartSize'*/
          currentFirstPartStart + 2 * currentPartSize >= sourceArrayLength
      ) {
        /*last available element index */
        sourceArrayLength - 1
      } else /*if (currentFirstPartStart + 2 * currentPartSize <
    sourceArrayLength)*/ {
        /*enough or more than 'currentPartSize' room / space / length left*/
        currentFirstPartStart + 2 * currentPartSize - 1
      }
    }
  }

  /*array content sorted as side effect (: Unit) no new collection created*/
  /*basically interesting in 'swapCount'*/
  /*Int 	32 bit signed value. Range -2147483648 to 2`147'483'647*/
  /*
  changing:
    >inner state of 'sourceArray'
    >'currentPartSize'
    >'firstPartStart'
  ? post or pre condition ?
  ? double initial (starting from 1) partSize
  until 'currentPartSize' < 'sourceArray'.length
    > calculate 'firstPartStart'
    if 'firstPartStart' > 'sourceArray'.length - 1
    leader is out of bound, so merge for 'currentPartSize' done
    then
      double 'currentPartSize'
    > until
      has second part to merge with first
      so 2nd part.size > 0
      or 2nd part end > 1st part end
      > 'mergeArraySortedParts'
  * */
  def emulateMergeSortForArray(
                                /*mutable object*/
                                sourceArray: Array[Int] = Array.emptyIntArray,
                                /*unchanging*/
                                /*'0' for emty*/
                                sourceArrayLength: Int = 0,
                                /*changing*/
                                /*to merge must have at least 1 element in
                                each part*/
                                currentPartSize: Int = 1,
                                /*changing*/
                                /*for empty array '0' is out of bound*/
                                /*`leader` position*/
                                firstPartStart: Int = 0,
                                /*changing*/
  /*64 bit signed value. -9223372036854775808 to 9223372036854775807*/
                                swapCount: Long = 0L
                                //): Array[Int] = {
                                ): Long = {
    /*? post condition or pre ?*/
    /*? 'PartSize' calculated before or after ?*/
    if (
    /*all sorted*/
      currentPartSize >= sourceArrayLength /*||
        evalNewPartsSize(currentPartSize) >= sourceArrayLength*/
    ) {
      /*return value */
      swapCount
    } else /*if (currentPartSize < sourceArrayLength)*/ {
      /*? post condition or pre ?*/
      if (
        firstPartStart >= sourceArrayLength ||
          /*initial first step*/
          currentPartSize == 0 /*||
          evalNewFirstPartStart(
                                 sourceArrayLength,
                                 firstPartStart,
                                 currentPartSize
                               ) >= sourceArrayLength*/
      ) {
        /*recursion*/
        /*with new `part size`*/
        emulateMergeSortForArray(
                                  /*mutable object*/
                                  sourceArray,
                                  /*unchanging*/
                                  sourceArrayLength,
                                  /*changing*/
                                  currentPartSize =
                                    evalNewPartsSize(currentPartSize),
                                  /*changing*/
                                  firstPartStart = 0,
                                  /*changing*/
                                  swapCount
                                )
      } else {
        /*first calculate merge parameters*/
        val currentFirstPartEnd =
          evalNewFirstPartEnd(
                               sourceArrayLength: Int,
                               firstPartStart: Int,
                               currentPartSize: Int
                             )
        val currentSecondPartEnd =
          evalNewSecondPartEnd(
                                sourceArrayLength: Int,
                                firstPartStart: Int,
                                currentPartSize: Int
                              )
        /*new first `part start`*/
        val newFirstPartEnd =
          evalNewFirstPartEnd(
                               sourceArrayLength: Int,
                               firstPartStart: Int,
                               currentPartSize: Int
                             )
        /*? may be out of range, greater / exceed the array length*/
        /*? use 'secondPartSize' instead as '0' means nothing to sort*/
        /*? or same as 'newSecondPartEnd == newFirstPartEnd'*/
        val newSecondPartEnd =
          evalNewSecondPartEnd(
                                sourceArrayLength: Int,
                                firstPartStart: Int,
                                currentPartSize: Int
                              )
        /*new merge only if 2nd part size > 0*/
        val newSwapCount: Long =
          if (currentSecondPartEnd > currentFirstPartEnd) {
            try {
              mergeArraySortedParts(
                                     /*unchanging*/
                                     sourceArray = sourceArray,
                                     /*unchanging*/
                                     sourceArrayLength = sourceArrayLength,
                                     /*changing ?*/
                                     currentPartSize =
                                       currentPartSize,
                                     /*changing ?*/
                                     firstPartStart = firstPartStart,
                                     /*changing ?*/
                                     firstPartEnd =
                                       currentFirstPartEnd,
                                     //newFirstPartEnd,
                                     /*'0' or greater*/
                                     firstPartSize =
                                       currentFirstPartEnd + 1 - firstPartStart,
                                     /*changing*/
                                     firstPartLeader = firstPartStart,
                                     /*may be useless / out of bound / range*/
                                     secondPartStart = currentFirstPartEnd + 1,
                                     /*changing ?*/
                                     secondPartEnd = currentSecondPartEnd,
                                     /*'0' or greater*/
                                     secondPartSize =
                                       currentSecondPartEnd -
                                         currentFirstPartEnd,
                                     /*changing*/
                                     //? redundant / useless ?
                                     //secondPartLeader: Int = 0,
                                     /*changing*/
                                     swapCount = swapCount
                                   )
            }
            catch {
              case e: StackOverflowError =>
                println("StackOverflowError")
                swapCount
              case e: Throwable => swapCount
              case e: Exception => swapCount
            }
          } else {
            swapCount
          }
        /*recursion*/
        /*with new first part start*/
        /*? must eventually exceed array upper bound to converge / end loop */
        val newFirstPartStart =
          /*out of bound or within indexes range*/
          currentSecondPartEnd + 1
          /*evalNewFirstPartEnd(
                               sourceArrayLength: Int,
                               firstPartStart: Int,
                               currentPartSize: Int
                             )*/

        try {
          emulateMergeSortForArray(
                                    /*mutable object*/
                                    sourceArray,
                                    /*unchanging*/
                                    sourceArrayLength,
                                    /*changing*/
                                    currentPartSize =
                                      currentPartSize,
                                    /*changing*/
                                    /*must be new to converge & break / stop
                                    loop*/
                                    firstPartStart =
                                      newFirstPartStart,
                                    /*evalNewFirstPartStart(
                                                           sourceArrayLength:
                                                            Int,
                                                           firstPartStart: Int,
                                                           currentPartSize: Int
                                                         ),*/
                                    /*changing*/
                                    swapCount = newSwapCount
                                  )
        }
        catch {
          case e: StackOverflowError =>
            println("StackOverflowError")
            newSwapCount
          case e: Throwable => newSwapCount
          case e: Exception => newSwapCount
        }
      }
    }
  }

  def countLines0(
                   inputLines: Iterator[String],
                   counter: Int = 0
                   ): Int = {
    if (inputLines.isEmpty) {
      /*return value*/
      counter
    } else {
      /*converge eventually / finally*/
      inputLines.next()
      /*recursion*/
      countLines0(
                   inputLines /*.drop(1)*/ ,
                   counter + 1
                 )
    }
  } /*work on small inputs*/
  //for (elem<-linesInInputStream) yield elem.toInt
  /*linesInInputStream
    .map(_.toString.toInt)*/

  def countLines(
                  inputLines: Iterator[String],
                  counter: /*Int*/ Long = 0L
                  ): /*Int*/ Long = {
    if (inputLines.hasNext /*isEmpty*/ ) {
      /*converge eventually / finally*/
      inputLines.next()
      /*recursion*/
      countLines(
                  inputLines /*.drop(1)*/ ,
                  counter + 1
                )
    } else {
      /*return value*/
      counter
    }
  } /*work on small inputs*/
  //for (elem<-linesInInputArray) yield elem.toInt*/
  //.map(_.toInt)
  //.foreach(_.toInt)
  //.toArray
  /*val linesInInputArray: Array[Int] =
    linesInInputStream
      .map(_.toInt)
      //.foreach(_.toInt)
      .toArray*/

  /*compare must precede*/
  def swapElements(
                    sourceArray: Array[Int],
                    indexOfLesser: Int,
                    indexOfGreater: Int
                    ): Unit = {
    val lesserElem: Int = sourceArray(indexOfLesser)
    val greaterElem: Int = sourceArray(indexOfGreater)
    /*side effect*/
    sourceArray(indexOfLesser) = greaterElem
    sourceArray(indexOfGreater) = lesserElem
  }

  /*? return last position of shifted element*/
  def shiftElemRightUntilOrdered(
                                  sourceArray: Array[Int],
                                  elemIndex: Int = 0,
                                  rangeStartIndex: Int = 0,
                                  rangeEndIndex: Int = 0,
                                  swapCount: Long = 0L
                                  ): Long = {
    if (
      elemIndex == rangeEndIndex ||
        sourceArray(elemIndex) <= sourceArray(rangeStartIndex)
    ) {
      /*return value*/
      //elemIndex
      swapCount
    } else /*if (
                 elemIndex < rangeEndIndex &&
                   sourceArray(elemIndex)>sourceArray(rangeStartIndex)
               )*/ {
      /*side effect*/
      //swapCount = swapCount + 1
      swapElements(
                    sourceArray: Array[Int],
                    indexOfLesser = rangeStartIndex,
                    indexOfGreater = elemIndex
                  )
      /*recursion*/
      shiftElemRightUntilOrdered(
                                  sourceArray: Array[Int],
                                  elemIndex =
                                    //rangeStartIndex/*elemIndex+1*/,
                                    if (rangeStartIndex < rangeEndIndex) {
                                      rangeStartIndex // + 1
                                    } else {
                                      rangeEndIndex
                                    },
                                  rangeStartIndex =
                                    if ((rangeStartIndex + 1) > rangeEndIndex) {
                                      rangeEndIndex
                                    } else {
                                      rangeStartIndex + 1
                                    },
                                  rangeEndIndex = rangeEndIndex,
                                  swapCount = swapCount + 1
                                )
    }

  }

  /*
  ?like merge sort ?
  one element considered be sorted
  two compared & swapped if needed
  then
  merge results
  */
  def divide(
              seqToDivide: Seq[Int],
              leftSide: Seq[Int],
              rightSide: Seq[Int]
              ): Unit = {

  }

  def merge(
             mergeLeftSide: Seq[Int] = /*Seq*/ Stream.empty[Int],
             mergeRightSide: Seq[Int] = /*Seq*/ Stream.empty[Int],
             mergeSortResult: SortedSeqProps =
             SortedSeqProps(
                             swapCount = 0,
                             sortedSeq = /*Seq*/ Stream.empty[Int]
                           )
             ): SortedSeqProps = {
    if (mergeLeftSide.isEmpty && mergeRightSide.isEmpty) {
      /*return value*/
      mergeSortResult
    } else {
      val (newLeft, newRight, newSwapCount, sorted) =
      /*assume that each side sorted already*/
        if (mergeLeftSide.nonEmpty && mergeRightSide.isEmpty) {
          (
            /*Seq*/ Stream.empty[Int],
            mergeRightSide,
            mergeSortResult.swapCount,
            /*append all that rest*/
            mergeSortResult.sortedSeq ++ mergeLeftSide
            )
        } else if (mergeLeftSide.isEmpty && mergeRightSide.nonEmpty) {
          (
            mergeLeftSide,
            /*Seq*/ Stream.empty[Int],
            mergeSortResult.swapCount,
            /*append all that rest*/
            mergeSortResult.sortedSeq ++ mergeRightSide
            )
        } else /*if (mergeLeftSide.nonEmpty && mergeRightSide.nonEmpty)*/ {
          if (mergeLeftSide.head <= mergeRightSide.head) {
            (
              mergeLeftSide.tail,
              mergeRightSide,
              mergeSortResult.swapCount,
              /*append element*/
              mergeSortResult.sortedSeq :+ mergeLeftSide.head
              )
          } else /*if (mergeLeftSide.head > mergeRightSide.head)*/ {
            (
              mergeLeftSide,
              mergeRightSide.tail,
              mergeSortResult.swapCount + 1,
              /*append element*/
              mergeSortResult.sortedSeq :+ mergeRightSide.head
              )
          }
        }
      /*recursion*/
      merge(
             mergeLeftSide = newLeft,
             mergeRightSide = newRight,
             mergeSortResult =
               SortedSeqProps(
                               swapCount = newSwapCount,
                               sortedSeq = sorted
                             )
           )
    }
  } /*merge work, not sure about 'swapCount' == `number of inversions`*/

  /*return 'swapCount'*/
  def mergeInArray(
                    sourceArray: Array[Int],
                    /*? equal sized or one size may be less than 'rangeSize' ?*/
                    /*as stop criteria*/
                    rangeSize: Int = 0,
                    /*may change within 'rangeSize' to maintain order in
                    merged parts*/
                    leftRangeSize: Int = 0,
                    leftRangeStart: Int = 0,
                    //leftRangeEnd: Int = 0,
                    rightRangeStart: Int = 0,
                    //rightRangeEnd: Int = 0,
                    swapCount: Long = 0L
                    ): /*(Array[Int],*/ Long /*)*/ = {
    if (
      rangeSize <= 0 ||
        (
          (leftRangeStart == -1 ||
            //leftRangeStart > leftRangeEnd
            leftRangeStart >= leftRangeStart + rangeSize
            ) &&
            rightRangeStart == -1
          )
    ) {
      /*return value*/
      swapCount
    } else {
      /*side effect*/
      val (newLeftStart, newRightStart, newSwapCount): (Int, Int, Long) =
      /*assume that each side sorted already*/
        if (
        /*at least one element*/
          (
            leftRangeStart > -1 &&
              //leftRangeStart <= leftRangeEnd
              leftRangeStart < leftRangeStart + rangeSize &&
              leftRangeStart < sourceArray.length
            ) &&
            /*empty, has no elements*/
            (rightRangeStart == -1 ||
              //rightRangeStart > rightRangeEnd
              rightRangeStart >= rightRangeStart + rangeSize ||
              rightRangeStart >= sourceArray.length
              )
        ) {
          /*side effect*/
          /*append all that rest in the left*/
          /*that means that all values in left indexes are already in order*/
          /*nothing to do*/
          (
            /*newLeftStart =*/ -1 /*leftRangeEnd + 1*/ ,
            rightRangeStart,
            swapCount
            )
        } else if (
               /*at least one element*/
                 (
                   rightRangeStart > -1 &&
                     //rightRangeStart <= rightRangeEnd
                     rightRangeStart < rightRangeStart + rangeSize &&
                     rightRangeStart < sourceArray.length
                   ) &&
                   /*empty, has no elements*/
                   (leftRangeStart == -1 ||
                     //leftRangeStart > leftRangeEnd
                     leftRangeStart >= leftRangeStart + rangeSize ||
                     leftRangeStart >= sourceArray.length
                     )
               ) {
          /*side effect*/
          /*append all that rest in / at the right*/
          /*that means that all values in right indexes range are already in
          order*/
          /*nothing to do*/
          (
            leftRangeStart,
            /*rightRangeStart =*/ -1 /*rightRangeEnd + 1*/ ,
            swapCount
            )
        } else if (
                 (
                   leftRangeStart > -1 &&
                     //leftRangeStart <= leftRangeEnd
                     leftRangeStart < leftRangeStart + rangeSize &&
                     leftRangeStart < sourceArray.length
                   ) &&
                   (
                     rightRangeStart > -1 &&
                       //rightRangeStart <= rightRangeEnd
                       rightRangeStart < rightRangeStart + rangeSize &&
                       rightRangeStart < sourceArray.length
                     )
               ) {
          if (
            sourceArray(leftRangeStart) <= sourceArray(rightRangeStart)
          ) {
            /*side effect*/
            /*that means that values in 'leftRangeStart' index is in order /
            sorted*/
            /*nothing to do*/
            (
              /*check next index value*/
              leftRangeStart + 1,
              rightRangeStart,
              swapCount
              )
          } else /*if (
          sourceArray(leftRangeStart) > sourceArray(rightRangeStart)
          )*/ {
            /*side effect*/
            /*swap needed*/
            /*swapElements(
                          sourceArray: Array[Int],
                          indexOfLesser = rightRangeStart,
                          indexOfGreater = leftRangeStart
                        )*/
            /*order in `rightRange` destroyed*/
            /*and must be restored*/
            (
              leftRangeStart + 1,
              rightRangeStart,
              /*
              one 'head' element from / as left vs previous right
              'rangeSize'
               */
              /*mergeInArray(
                            sourceArray: Array[Int],
                            rangeSize = rangeSize - 1,
                            /*special case*/
                            leftRangeSize = 1,
                            leftRangeStart = rightRangeStart,
                            //leftRangeEnd = rightRangeStart,
                            rightRangeStart = rightRangeStart + 1,
                            //rightRangeEnd = rightRangeEnd,
                            swapCount = swapCount + 1
                          )*/
              shiftElemRightUntilOrdered(
                                          sourceArray =
                                            sourceArray,
                                          elemIndex = leftRangeStart,
                                          rangeStartIndex = rightRangeStart,
                                          rangeEndIndex =
                                            if (
                                              rightRangeStart + rangeSize <=
                                                sourceArray.length
                                            ) {
                                              rightRangeStart + rangeSize
                                            } else {
                                              /*last / max index*/
                                              sourceArray.length - 1
                                            },
                                          swapCount = swapCount
                                        )
              )
          }
        } else {
          println("unexpected condition")
          /*return value*/
          ( /*leftRangeStart*/ -1,
            /*rightRangeStart*/ -1,
            swapCount)
        }
      /*recursion*/
      mergeInArray(
                    sourceArray: Array[Int],
                    rangeSize = rangeSize * 2,
                    /*common case*/
                    leftRangeSize = rangeSize,
                    leftRangeStart = newLeftStart,
                    //leftRangeEnd = leftRangeEnd,
                    rightRangeStart = newRightStart,
                    //rightRangeEnd = rightRangeEnd,
                    swapCount = newSwapCount
                  )
    }
  }

  def mergeSort(
                 unSortedSeq: /*Seq*/ Stream[Int],
                 /*here initial 'unSortedSeq' goes*/
                 /*leftSide: Seq[Int] = Seq.empty[Int],
                 rightSide: Seq[Int] = Seq.empty[Int],*/
                 sortResult: SortedSeqProps =
                 SortedSeqProps(
                                 swapCount = 0,
                                 sortedSeq = /*Seq*/ Stream.empty[Int]
                               )
                 ): SortedSeqProps = {
    //if (unSortedSeq.isEmpty) {
    if (
      unSortedSeq.isEmpty ||
        //unSortedSeq.tail == Nil
        unSortedSeq.tail.isEmpty
    ) {
      /*return value*/
      //sortResult
      SortedSeqProps(
                      swapCount = sortResult.swapCount,
                      sortedSeq = unSortedSeq
                    )
    } else {
      merge(
             mergeLeftSide =
               mergeSort(
                          unSortedSeq =
                            unSortedSeq.take(unSortedSeq.size / 2)
                        ).sortedSeq,
             mergeRightSide =
               mergeSort(
                          unSortedSeq =
                            unSortedSeq.drop(unSortedSeq.size / 2)
                        ).sortedSeq
           )
    }
  }

  def mergeSortArray(
                      /*? content changes, length stay same ?*/
                      unSortedArray: Array[Int],
                      swapCount: Long = 0L,
                      nextMergeStart: Int = 0
                      //nextMergeSize: Int = 1//nextMergeSize * 2
                      ): /*(Array[Int],*/ Long /*)*/
  /*SortedSeqProps*/ = {
    /*? stop criteria ?*/
    if (
      unSortedArray.isEmpty ||
        nextMergeStart >= unSortedArray.length
    //nextMergeSize >= unSortedArray.length
    ) {
      /*return value*/
      //sortResult
      /*(
        unSortedArray,*/
      swapCount
      //)
    } else {
      val arrayLength = unSortedArray.length
      /*recursion*/
      mergeInArray(
                    unSortedArray: Array[Int],
                    leftRangeStart = nextMergeStart,
                    //leftRangeEnd = arrayLength / 2,
                    rightRangeStart = arrayLength / 2 + 1,
                    //rightRangeEnd =
                    /*last possible index*/
                    //arrayLength - 1,
                    swapCount = swapCount
                    /*mergeLeftSide =
                      mergeSort(
                                 unSortedSeq =
                                   unSortedSeq.take(unSortedSeq.size / 2)
                               ).sortedSeq,
                    mergeRightSide =
                      mergeSort(
                                 unSortedSeq =
                                   unSortedSeq.drop(unSortedSeq.size / 2)
                               ).sortedSeq*/
                  )
    }
  }

  case class SortedSeqProps(
                             swapCount: Int,
                             sortedSeq: /*Seq*/ Stream[Int]
                             )

}

/*companion*/
object InversionsNumber {

  import InversionsNumber._

}

//import testInversionsNumber.InversionsNumberSuit

/*unit test*/
object Main extends App {

  //import InversionsNumber._
  //testInversionsNumber
  //testInversionsNumber.InversionsNumberSuit
  //import test.testInversionsNumber.InversionsNumberSuit

  /*unit test*/
  val listOfInt =
    List(3, 5, 9, 11, 15, 19, 21)
  val listOfStr =
    listOfInt
    .map(_.toString)
  val it = listOfInt.iterator
  val inversionsNumber = new InversionsNumber
  val testArray: Array[Int] =
    listOfInt.toArray

  Console.println(listOfInt)
  //val inversionsNumber = new InversionsNumber
  /*lazy*/
  /*val inputLines =
    inversionsNumber
      .getInput()*/

  /*lazy*/
  /*val inputLinesArray: Array[String/*Int*/] =
    inputLines
      .toArray*/
  //.map(_.toInt)

  Console.print("Any")
  /*println(
           s"linesInInput: ${
             inversionsNumber
               .countLines(
                 it
                          )
           } =? 7"
         )*/
  /*println(
           s"inputLines: ${
                 inputLines
           } =? "
         )
  println(
           s"inputLines.length: ${
                 inputLines.length
           } =? "
         )
  println(
           s"inputLinesArray.length: ${
                 inputLinesArray.length
           } =? "
         )
  println(
           s"inputLinesArray.head: ${
                 inputLinesArray.head
           } =? "
         )*/
  Console
  .println(
      s"linesInInputArray.tail.head: ${
        inversionsNumber
        //.linesInInputArray
        .integersInInputSeq
        .tail
        .head
      } =? "
          )
  Console
  .println(
      s"inputLinesArray.last: ${
        inversionsNumber
        .integersInInputArray
        .last
      } =? '91901'"
          )
  /*println(
           s"linesInInput: ${
             inversionsNumber
               .countLines(
                 inputLines
                          )
           } =? "
         )*/
  //println(s"linesInInput: ${inversionsNumber.linesInInput} =? 100 000")
  println(
           s"mergeSort(): ${
             inversionsNumber
             .mergeSort(
                 unSortedSeq =
                   //Seq(2,1)
                   listOfInt.reverse.toStream
                       )
           } =? "
         )

  /*println(
           s"mergeSort(integersInInputSeq).swapCount: ${
             inversionsNumber
               .mergeSort(
                 unSortedSeq =
                   //Seq(2,1)
                   //listOfInt.reverse
                   inversionsNumber
                     .inegersInInputStream
                 //.integersInInputArray
                 //.integersInInputSeq.take(5000)
                         ).swapCount
           } =? "
         )*/
  println(
           s"mergeInArray(5,6,7,1,2,3) swapCount: ${
             inversionsNumber
             .mergeInArray(
                 //testArray,
                 Array(5, 6, 7, 1, 2, 3),
                 3,
                 3,
                 0,
                 3
                          )
           } =? "
         )

  val testEndBeacon = true
}
