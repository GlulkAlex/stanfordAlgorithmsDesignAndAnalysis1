package QuickSortComparisonsPQ2

//import inversionsNumberPQ1

/**
 * Created by gluk-alex on 7/17/15.
 */
object Pivoting1FirstElement {

  /*if path & name OK this is enough*/
  def getInput(
                filename: String =
                "QuickSort.txt",
                filePath: String =
                "/media/gluk-alex/GDI/Java/Scala/sbt/projects/" +
                  "stanfordAlgorithmsDesignAndAnalysis1/src/test/scala/" +
                  "testQuickSortComparisons/"
                ): Iterator[String] = {
    //"/media/gluk-alex/GDI/Java/Scala/sbt/projects/
    // stanfordAlgorithmsDesignAndAnalysis1/src/
    // test/scala/testQuickSortComparisons"
    /*val filename =
      "QuickSort.txt"
    val filePath =
      "/media/gluk-alex/GDI/Java/Scala/sbt/projects/" +
    "stanfordAlgorithmsDesignAndAnalysis1/src/test/scala/" +
    "testQuickSortComparisons/"*/
    /*val currFile = Source
      .fromFile(filePath + filename)*/
    val currFileLines =
      scala.io.Source
      .fromFile(filePath + filename)
      .getLines()
    /*return value*/
    currFileLines
  }

  /*
  Question 1
  GENERAL DIRECTIONS:
  Download the text file
  /media/gluk-alex/GDI/Java/Scala/sbt/projects/
  stanfordAlgorithmsDesignAndAnalysis1/src/test/scala/
  testQuickSortComparisons/
  QuickSort.txt.

  The file contains
  all of the integers between 1 and 10,000 (inclusive, with no repeats)
  in unsorted order.
  The integer in the i-th row of the file
  gives you
  the i-th entry of an input array.
  TODO
  Your task is to
  compute
  the `total number of comparisons` used to
  sort the given input file by QuickSort.
  As you know,
  the number of comparisons depends
  on which elements are chosen as pivots,
  so
  we'll ask you to
  explore three different `pivoting rules`.
  You should
  not count comparisons one-by-one.
  Rather,
  when there is a recursive call on a subarray of length 'm',
  you should
  simply add 'm−1' to your running `total of comparisons`.
  (This is because
  the pivot element is
  compared to each of the other 'm−1' elements in
  the subarray in this recursive call.)

  WARNING:
  >The Partition subroutine can
    be implemented in several different ways, and
    different implementations can
    give you differing numbers of comparisons.
    For this problem,
    you should
    implement the Partition subroutine exactly as
    it is described in the video lectures
    (otherwise you might get the wrong answer).

  DIRECTIONS FOR THIS PROBLEM:
  >For the first part of the programming assignment,
    you should
    always use
    the `first element` of the array as the `pivot element`.

  HOW TO GIVE US YOUR ANSWER:
  >Type the numeric answer in the space provided.
  So if your answer is '1198233847', then
  just type '1198233847' in the space provided
  without any space / commas / other punctuation marks.
  You have 5 attempts to get the correct answer.
  (We do not require you to submit your code, so
  feel free to use the programming language of your choice,
  just type the numeric answer in the following space.)
   */

  /*compare must precede*/
  /*? suitable for other Sequence too ?*/
  def swapArrayElements(
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

  def placePivotAtStart(
                         sourceArray: Array[Int],
                         headIndex: Int,
                         pivotIndex: Int
                         ): /*Unit*/ Array[Int] = {
    val pivot: Int =
      sourceArray(pivotIndex)

    if (sourceArray(headIndex) == pivot) {
      /*we good, nothing to do*/
    } else /*if (sourceArray.head > pivot)*/ {
      /*swap*/
      swapArrayElements(
                         sourceArray: Array[Int],
                         indexOfLesser = headIndex,
                         indexOfGreater = pivotIndex
                       )
    }
    /*return value*/
    sourceArray
  }

  /*reserve additional extra space*/
  def PivotingArrayToEmptyClone(
                                 sourceArray: Array[Int],
                                 sourceArrayLenght: Int,
                                 pivotIndex: Int
                                 ): Array[Int] = {
    /*base case when 'pivotIndex == 0' or `pivot element` is 'head'*/
    /*so, reduce to that case*/
    val pivot: Int =
      sourceArray(pivotIndex)

    if (sourceArray.head == pivot) {
      /*we good, nothing to do*/
    } else /*if (sourceArray.head > pivot)*/ {
      /*swap*/
      swapArrayElements(
                         sourceArray: Array[Int],
                         indexOfLesser = 0,
                         indexOfGreater = pivotIndex
                       )
    }

    val pivotedArray: Array[Int] =
      new Array[Int](sourceArrayLenght)
    /*side effects*/
    /*increasing up to Array Upper bound*/
    var freeLeftIndex = 0
    /*decreasing down to Array Lower bound*/
    var freeRightIndex = sourceArrayLenght - 1
    /*bucket up*/
    for (i <- sourceArray.indices /*if i>0*/ ) {
      //if (sourceArray(i)<=pivot) {
      if (sourceArray(i) < pivot) {
        /*go to next free left index*/
        pivotedArray(freeLeftIndex) = sourceArray(i)
        freeLeftIndex += 1
      } else if (sourceArray(i) > pivot) {
        /*go to next free right index*/
        pivotedArray(freeRightIndex) = sourceArray(i)
        freeRightIndex -= 1
      } else /*if (sourceArray(i)==pivot)*/ {
        /*go to next free left index at last step*/
      }
    }
    pivotedArray(freeLeftIndex) = pivot
    /*return value*/
    //(lessThanPivot, pivot, greaterThanPivot)
    pivotedArray
  }

  /*
  be sure to
  implement Partition exactly
  as described in the video lectures
  (including
  exchanging the pivot element with
  the first element
  just before the main Partition subroutine).
   */
  /*constant space - no additional extra space needed*/
  /* ? prerequisites - pivot at 'startingIndex' ?*/
  def PivotingArrayInPlace(
                            sourceArray: Array[Int],
                            startingIndex: Int,
                            endingIndex: Int,
                            /*mast be within*/
                            pivotIndex: Int
                            ): Array[Int] = {
    assume(pivotIndex >= startingIndex && pivotIndex <= endingIndex,
           "'pivotIndex' mast be within range")

    placePivotAtStart(
                       sourceArray: Array[Int],
                       headIndex = startingIndex,
                       pivotIndex: Int
                     )
    val pivot: Int = sourceArray(startingIndex)
    /*side effects*/
    /*? must point to the next after it ?*/
    var lessThanPivotEndIndex: Int =
      startingIndex + 1
    /*? must point to the next after it ?*/
    val greaterThanPivotEndIndex: Int =
      lessThanPivotEndIndex + 1

    for (i <- greaterThanPivotEndIndex to endingIndex) {
      if (sourceArray(i) < pivot) {
        /*? may be check for redundant swaps in first / initial steps?*/
        //if (lessThanPivotEndIndex < i) {}
        swapArrayElements(
                           sourceArray,
                           lessThanPivotEndIndex,
                           /*greaterThanPivotEndIndex*/ i)
        lessThanPivotEndIndex += 1
      } else if (sourceArray(i) > pivot) {
        /*do nothing*/

      } else /*if(sourceArray(i)==pivot)*/ {
        /*?do nothing?*/
        /*assume that elements are distinct*/

      }
    }
    /*swap pivot from head to the right place*/
    swapArrayElements(
                       sourceArray,
                       indexOfLesser = startingIndex,
                       indexOfGreater = lessThanPivotEndIndex - 1
                     )
    /*return value*/
    sourceArray
  }

  /*? assume 'sourceSeq.nonEmpty' ?*/
  def ChoosePivotIndex(
                        sourceSeq: Array[Int],
                        sourceSeqLenght: Int
                        ): Int = {
    /*return value*/
    sourceSeqLenght
  }

  def ChooseFirstElementAsPivot(
                                 sourceSeq: Array[Int],
                                 sourceSeqLenght: Int
                                 ): Int = {
    if (sourceSeq.isEmpty) {
      /*return value*/
      /*assume that all values must be positive*/
      -1
    } else {
      /*return value*/
      sourceSeq.head
    }
  }

  def ChooseLastElementAsPivot(
                                sourceSeq: Array[Int],
                                sourceSeqLenght: Int
                                ): Int = {
    if (sourceSeq.isEmpty) {
      /*return value*/
      /*assume that all values must be positive*/
      -1
    } else {
      /*return value*/
      sourceSeq.last
    }
  }

  def ChooseMedianOfThreeAsPivot(
                                  sourceSeq: Array[Int],
                                  //sourceSeqLenght: Int,
                                  firstSeqIndex: Int,
                                  lastSeqIndex: Int
                                  ): Int = {
    /*for even '4 5 6 7' 'middleIndex = 2' 'elem = 5'*/
    /*'4/2' or 'length/2'*/
    /*for odd '3 4 5 6 7' 'middleIndex = 2' 'elem = 5'*/
    /*'5/2' or 'length/2'*/
    /*? if firstSeqIndex == lastSeqIndex then == middleIndex ? */
    //0,(1,[2],3,4),5
    //(1) + (4-1)/2
    //0,1,(2,3,[4],5,6),7
    //(2) + (6-2)/2
    //([0])
    //(0) + (0-0)/2
    //([0],1)
    //(0) + (1-0)/2
    //0,1,2,3,([4],5),6,7
    //(4) + (5-4)/2
    val middleIndex: Int =
      if (
        sourceSeq.isEmpty
      ) {
        firstSeqIndex
      } else {
        firstSeqIndex + (lastSeqIndex - firstSeqIndex) / 2
      }
    val (firstMiddleMin, firstMiddleMax): (Int, Int) =
      if (
        sourceSeq(middleIndex) >= sourceSeq(firstSeqIndex)
      ) {
        (sourceSeq(firstSeqIndex), sourceSeq(middleIndex))
      } else {
        (sourceSeq(middleIndex), sourceSeq(firstSeqIndex))
      }
    val median: Int =
    //List(firstSeqIndex,middleIndex,lastSeqIndex).sorted.apply(1)
      if (
        firstMiddleMax <= sourceSeq(lastSeqIndex)
      ) {
        firstMiddleMax
      } else /*if (
        firstMiddleMax > sourceSeq(lastSeqIndex)*/ {
        if (sourceSeq(lastSeqIndex) >= firstMiddleMin) {
          sourceSeq(lastSeqIndex)
        } else {
          firstMiddleMin
        }
      }

    if (sourceSeq.isEmpty) {
      /*return value*/
      /*assume that all values must be positive*/
      -1
    } else {
      /*return value*/
      median
    }
  }

  def ChoosePivot(
                   sourceSeq: Array[Int],
                   sourceSeqLenght: Int
                   ): Int = {
    if (sourceSeq.isEmpty) {
      /*return value*/
      /*assume that all values must be positive*/
      -1
    } else {
      /*return value*/
      sourceSeq.head
    }
  }

  /*input has only positive distinct integers*/
  def QuickSort(
                 unsorted: Array[Int],
                 unsortedLenght: Int
                 ): Array[Int] = {
    if (unsortedLenght <= 1) {
      /*return value*/
      unsorted
    } else {
      //val pivotIndex: Int =
      val pivot: Int =
        ChoosePivot(
                     sourceSeq = unsorted,
                     sourceSeqLenght = unsortedLenght
                   )
      val (part1, part2) =
        unsorted
        //.partition(_ <= unsorted(pivotIndex))
        .partition(_ <= pivot)

      /*recursion*/
      val part1Sorted =
        QuickSort(
                   /*? 'part1.head == pivot', so exclude 'pivot' ?*/
                   unsorted = part1.tail,
                   unsortedLenght = part1.tail.length
                 )
      val part2Sorted =
        QuickSort(
                   unsorted = part2,
                   unsortedLenght = part2.length
                 )
      /*return value*/
      //part1Sorted +: pivot ++ part2Sorted
      part1Sorted ++ (pivot +: part2Sorted)
    }

  }

  trait PivotRule

  case object FirstPivot extends PivotRule

  case object LastPivot extends PivotRule

  case object MedianPivot extends PivotRule
  /*after using get java.lang.StackOverflowError*/
  case class SortResults(sortedArray: Array[Int], comparisonsTotal: Int)

  /*input has only positive distinct integers*/
  /* using three different pivot rule*/
  def QuickSortComparisons(
                            unsorted: Array[Int],
                            unsortedLenght: Int,
                            /*accumulator*/
                            comparisonsTotal: Int = 0,
                            pivotRule: PivotRule = FirstPivot
                            ): SortResults = {
    if (unsortedLenght <= 1) {
      /*return value*/
      //unsorted
      //0
      //unsortedLenght - 1
      SortResults(unsorted, comparisonsTotal)
    } else {
      //val pivotIndex: Int =
      val pivot: Int =
        pivotRule match {
          case FirstPivot => {
            ChooseFirstElementAsPivot(
                                       sourceSeq = unsorted,
                                       sourceSeqLenght = unsortedLenght
                                     )
          }
          case LastPivot  => {
            ChooseLastElementAsPivot(
                                      sourceSeq = unsorted,
                                      sourceSeqLenght = unsortedLenght
                                    )
          }
          case MedianPivot  => {
            ChooseMedianOfThreeAsPivot(
                                      sourceSeq = unsorted,
                                      firstSeqIndex = 0,
                                      lastSeqIndex = unsortedLenght - 1
                                    )
          }
        }
      /*
      TODO
      fix partition logic for each `pivotRule`
      * */
      /*has case for each pivotRule*/
      val (part1, part2) =
        unsorted
        //.tail
        .init
        //.partition(_ <= unsorted(pivotIndex))
        .partition(_ <= pivot)

      /*recursion*/
      //val part1Sorted =
      /*comparisonsTotal = comparisonsTotal + part1.tail.length - 1*/
      //val (part1Sorted, part1Comparisons): (Array[Int], Int) =
      val SortResults(part1Sorted, part1Comparisons): SortResults =
        QuickSortComparisons(
                              /*? 'part1.head == pivot', so exclude 'pivot' ?*/
                              unsorted =
                                //part1.tail,
                                part1,
                              unsortedLenght =
                                //part1.tail.length,
                                part1.length,
                              comparisonsTotal +
                                //part1.tail.length - 1
                                part1.length - 1,
                              pivotRule = pivotRule
                            )
      //val part2Sorted =
      /*comparisonsTotal = comparisonsTotal + part2.length - 1*/
      //val (part2Sorted, part2Comparisons): (Array[Int], Int) =
      val SortResults(part2Sorted, part2Comparisons): SortResults =
        QuickSortComparisons(
                              unsorted = part2,
                              unsortedLenght = part2.length,
                              comparisonsTotal + part2.length - 1,
                              pivotRule = pivotRule
                            )
      /*return value*/
      //part1Sorted +: pivot ++ part2Sorted
      SortResults(
        part1Sorted ++ (pivot +: part2Sorted),
        part1Comparisons + part2Comparisons
        )
    }

  }
}

/*
Question 2
GENERAL DIRECTIONS AND HOW TO GIVE US YOUR ANSWER:
See the first question.

DIRECTIONS FOR THIS PROBLEM:
>Compute the number of comparisons (as in Problem 1),
always using
the `final element` of the given array as
the `pivot element`.
Again,
be sure to implement the Partition subroutine exactly as
it is described in the video lectures.
Recall from the lectures that,
just before the main Partition subroutine,
you should
`exchange` the `pivot element`
(i.e., the `last element`)
with the `first element`.
 */

/*
Question 3
GENERAL DIRECTIONS AND HOW TO GIVE US YOUR ANSWER:
See the first question.

DIRECTIONS FOR THIS PROBLEM:
>Compute the number of comparisons (as in Problem 1),
using
the "median-of-three" pivot rule.
[The primary motivation behind this rule is
to do a little bit of extra work to
get much better performance on input arrays that are
nearly sorted or
reverse sorted.]
In more detail,
you should
choose the `pivot` as follows.
Consider
the `first`,
`middle`, and
`final` 'elements' of the given array.
(If the array has
`odd` 'length' it should be clear
what the "middle" element is;
for an array with `even` 'length' '2k',
use
the 'k-th' element as the "middle" element.
So
for the array '4 5 6 7',
the "middle" element is
the second one ---- '5' and not 6!)
Identify
which of these three elements is the `median`
(i.e., the one whose value is in between the other two), and
use this as your `pivot`.
As discussed in the first and
second parts of this programming assignment,
be sure
to implement Partition exactly
as described in the video lectures
(including
`exchanging` the pivot element with
the `first` element just before
the main `Partition subroutine`).

EXAMPLE:
For the input array
8 2 4 5 7 1
you would
consider
the first (8),
middle (4), and
last (1) elements;
since '4' is
the median of the set {1,4,8},
you would
use '4' as your `pivot element`.

SUBTLE POINT:
>A careful analysis would
  keep track of
  the `comparisons` made
  in identifying
  the `median` of the `three candidate` elements.
  You should NOT do this.
  That is,
  as in the previous two problems,
  you should
  simply add 'm−1' to
  your running total of comparisons
  every time you recurse on a subarray with length 'm'.
 */