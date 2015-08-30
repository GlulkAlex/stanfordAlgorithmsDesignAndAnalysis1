package testMedianMaintenance

import medianMaintenance.MedianMaintenance.HeapsMedian
import org.scalatest.FunSuite

import java.text.SimpleDateFormat
import java.util.Calendar

import filesIO.FilesIO._
import medianMaintenance._

/**
 * Created by gluk-alex on 8/29/15.
 */
class MedianMaintenanceSuit
  extends FunSuite {
  test(
        "1: 'HeapsMedian.getMedian'" +
          "should " +
          "return right median on / of odd number of elements"
      ) {
          val inputEndSample: String =
            "2366\n558\n3536\n4855\n5940"
          /*val inputIter:
          Iterator[String] =
            inputEndSample
            .split("\n")
            .toIterator*/
          val (inputIter, inputIterCopy):
          (Iterator[String], Iterator[String]) =
            inputEndSample
            .split("\n")
            .toIterator
            .duplicate
          val heapsMedian: HeapsMedian =
            new HeapsMedian
          /*side effect*/
          //for
          inputIter.
          foreach { s => heapsMedian.add(s.toInt) }
          val answerSample: Option[Int] =
            heapsMedian.getMedian
          val inputList: List[Int] =
            inputIterCopy
            .toList
            .map(_.toInt)
            .sorted
          val expectedAnswer: Option[Int] =
            Some(
                  //3536
                  inputList
                  .apply((inputList.length + 1) / 2 - 1)
                )
          println(s"'inputList' is:" + inputList.mkString(","))
          println(s"'heapsMedian' is:" + heapsMedian.showContent)
          println(s"'expectedAnswer' is:" + expectedAnswer)

          assume(answerSample == expectedAnswer,
                 s"must be equal")
        }
  test(
        "2: 'HeapsMedian.getMedian'" +
          "should " +
          "return right median on / of `even` number of elements"
      ) {
          val inputEndSample: String =
            "2366\n558\n3536\n4855\n"
          val (inputIter, inputIterCopy):
          (Iterator[String], Iterator[String]) =
            inputEndSample
            .split("\n")
            .toIterator
            .duplicate
          val heapsMedian: HeapsMedian =
            new HeapsMedian
          /*side effect*/
          //for
          inputIter.
          foreach { s => heapsMedian.add(s.toInt) }
          val answerSample: Option[Int] =
            heapsMedian
            .getMedian
          val inputList: List[Int] =
            inputIterCopy
            .toList
            .map(_.toInt)
            .sorted
          val expectedAnswer: Option[Int] =
            Some(
                  inputList
                  .apply(inputList.length / 2 - 1)
                )
          println(s"'inputList' is:" + inputList.mkString(","))
          println(s"'expectedAnswer' is:" + expectedAnswer)
                println(s"'heapsMedian' is:" + heapsMedian.showContent)

          assume(answerSample == expectedAnswer,
                 s"must be equal")
        }
}
