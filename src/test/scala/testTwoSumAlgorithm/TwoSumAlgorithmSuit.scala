package testTwoSumAlgorithm

import org.scalatest.FunSuite

import java.text.SimpleDateFormat
import java.util.Calendar

import filesIO.FilesIO._
import stronglyConnectedComponentsPQ4.ShowProgress._
import twoSUM_Algorithm.TwoSumAlgorithm._

/**
 * Created by gluk-alex on 8/29/15.
 */
class TwoSumAlgorithmSuit
  extends FunSuite {
  test(
        "1: 'collectSumWithinInterval'" +
          "should " +
          "return collected sums " +
          "of two distinct number from input" +
          " that stays within defined interval"
      ) {
          val inputTakeNumber: Int =
            //10000
          /*3:m/32:s/738:milliSeconds for Set[Long]*/
            /*4:m/5:s/445:milliSeconds
            4:m/37:s/521:milliSeconds
            with immutable.TreeSet[Long]*/
            //100000
          /*
          1:h/28:m/16:s/967:milliSeconds
          'heapSet.size' is:999752
          'answer' is: four two seven
           */
            1000000
          val inPutFilePath: String =
          //"/home/gluk-alex/Documents/sbt_projects/"
            "/media/gluk-alex/" +
              "GDI/Java/Scala/sbt/projects/" +
              "stanfordAlgorithmsDesignAndAnalysis1/" +
              "src/test/scala/" +
              //"testSCC/"
              //"testMedianMaintenance/"
                "testTwoSumAlgorithm/"
          val inPutFileName: String =
          "algo1-programming_prob-2sum.txt"
            //"Median.txt"
          lazy val (fileContentIter, fileContentIterCopy):
          (Iterator[String], Iterator[String]) =
          //Iterator.empty
            readFromFile(
                          fileName = inPutFileName,
                          filePath = inPutFilePath
                        )
            /*reduce / control input size*/
            //.take(inputTakeNumber)
            .duplicate
          /*val inputEndSample: String =
            "2366\n558\n3536\n4855\n"*/
          val expectedInputSize: Int =
            1.0E6.toInt
          /*val (inputIter, inputIterCopy):
          (Iterator[String], Iterator[String]) =
            inputEndSample
            .split("\n")
            .toIterator
            .duplicate*/
          val heapSet:
          //scala.collection.mutable.
          Set[Long] =
          /*scala.collection.immutable.
          TreeSet[Long] =*/
            //scala.collection.mutable.
            //Set(-5L, -4L, -3L, -2L, -1L, 0L, 1L, 2L, 3L, 4L, 5L)
            /*scala.collection.immutable.
            TreeSet.empty[Long] ++*/
            //.apply(
            fileContentIter
            .map(_.toLong)
            .toStream
                  //)
            .toSet
            //.toTreeSet
          /*heapSet
            /*return Set.empty or Set that is .nonEmpty*/
          .collect{case y if y == 0 => y}*/
          /*lazy*/ val startDateTime1:
          java.util.Date =
            Calendar.getInstance().getTime()
          /*lazy*/ val startTimeStamp1: Long =
            System.currentTimeMillis
          /*Date and Time Pattern */
          val timeStampFormat = new SimpleDateFormat("HH:mm:ss.SSS")
          lazy val startStampString1 =
            timeStampFormat.format(startDateTime1)
          println(s"'collectSumWithinInterval' started at:" +
                    startStampString1)
          val sumsCollection:
          scala.collection.mutable.
          Set[Long] =
          //Stream[Long] =
            collectSumWithinInterval(
                                      sourceSet = heapSet,
                                      remainsToCheckSet =
                                        heapSet.iterator/*,
                                      minT = -2L,
                                      maxT = 2L*/
                                    )
          lazy val endDateTime1 = Calendar.getInstance().getTime()
          lazy val endTimeStamp1: Long = System.currentTimeMillis()
          lazy val endStampString1 = timeStampFormat.format(endDateTime1)
          println(s"Done at:" + endStampString1)
          println(
                   s"time difference is:" +
                    (endTimeStamp1 - startTimeStamp1) + " Millis or:" +
                    convertLongToTimeString(
                                             timeNumberMillis =
                                               endTimeStamp1 -
                                                 startTimeStamp1,
                                             colored = false
                                           )
                 )
                val answer: Long =
                  sumsCollection
            .size
          val expectedAnswer: Option[Int] = None
          println(s"'expectedInputSize' is:" + expectedInputSize)
          println(s"'fileContentIterCopy.size' is:" + fileContentIterCopy.size)
          println(s"'heapSet.size' is:" + heapSet.size)
          println(s"'answer' is:" + answer)
          println(s"'sumsCollection' is:" +
                    sumsCollection.take(20).mkString(","))
          //println(s"'expectedAnswer' is:" + expectedAnswer)
          //println(s"'heapsMedian' is:" + heapsMedian.showContent)

          assume(
                  //numeric answer (an integer between 0 and 20001)
                  answer >= 0 &&
                  answer < 20001,
                 s"must be between '0' and '20001'")
        }
  ignore(
        "2: 'collectSumWithinInterval' using 'for' & 'filter'" +
          "should " +
          "return collected sums " +
          "of two distinct number from input" +
          " that stays within defined interval"
      ) {
                val heapSet: Set[Long] =
                Set(-5L, -4L, -3L, -2L, -1L, 0L, 1L, 2L, 3L, 4L, 5L)
                val minT: Long = -2L
                val maxT: Long = 2L

                case class Sums(x: Long, y: Long, sum: Long)

                val possibleSums:
                //Set[Sums] =
                //List[Sums] =
                List[Long] =
                  (for {
                  x <- heapSet
                  y <- heapSet if x != y && x + y >= minT && x + y <= maxT
                } yield {
                  x + y
                  //Sums(x, y, x + y)
                })
                .toList
                println(s"'possibleSums' is:" +
                          possibleSums
                          .take(20)
                          /*.map(s=>
                                 s"[x:" + s.x +
                                   ",y:" + s.y +
                                   ",sum:" + s.sum + "]")*/
                          .mkString("{", ",", "}"))

                assume(
                        possibleSums.size >= 0 &&
                          possibleSums.size < maxT - minT + 1 + 1,
                        s"must be between '0' and '5 + 1'")

              }
  ignore(
        "3: 'collectSumWithinInterval' using 'combinations'" +
          "should " +
          "return collected sums " +
          "of two distinct number from input" +
          " that stays within defined interval"
      ) {
                val heapSet: Set[Long] =
                Set(-5L, -4L, -3L, -2L, -1L, 0L, 1L, 2L, 3L, 4L, 5L)
                val minT: Long = -2L
                val maxT: Long = 2L

                case class Sums(x: Long, y: Long, sum: Long)

                val possibleCombinations:
                List[List[Long]] =
                  heapSet
                .toList
                  .combinations(2)
                .toList

                val possibleSums:
                //Set[Sums] =
                //List[Sums] =
                List[Long] =
                //.permutations
                //.sliding(2)
                  possibleCombinations
                .view
                .collect{case List(x, y) if
                  x != y && x + y >= minT && x + y <= maxT =>
                    x + y}
                  .toList

                println(
                         s"'possibleCombinations.size' is:" +
                          possibleCombinations.length
                       )
                println(
                         s"first '20' elements from " +
                           s"'possibleCombinations' are:\n" +
                          possibleCombinations
                          .take(20)
                          .map(_.mkString("[", ",", "]"))
                          /*.map(s=>
                                 s"[x:" + s.x +
                                   ",y:" + s.y +
                                   ",sum:" + s.sum + "]")*/
                          .mkString("{", ",", "}")
                )
                println(
                         s"'possibleSums.size' is:" +
                           possibleSums.length
                       )
                println(s"first '20' elements from 'possibleSums' are:" +
                          possibleSums
                          .take(20)
                          /*.map(s=>
                                 s"[x:" + s.x +
                                   ",y:" + s.y +
                                   ",sum:" + s.sum + "]")*/
                          .mkString("{", ",", "}"))
                println(
                         s"first '20' `distinct` elements " +
                           s"from 'possibleSums' are:" +
                          possibleSums
                            .distinct
                          .take(20)
                          /*.map(s=>
                                 s"[x:" + s.x +
                                   ",y:" + s.y +
                                   ",sum:" + s.sum + "]")*/
                          .mkString("{", ",", "}"))

                assume(
                        possibleCombinations.nonEmpty,
                        s"must be nonEmpty")

              }
  /*!!!Warn!!! to slow, even on '10000' numbers*/
  ignore(
        "4: 'collectSumWithinInterval' using 'combinations' & 'Stream'" +
          "should " +
          "return collected sums " +
          "of two distinct number from input" +
          " that stays within defined interval"
      ) {
                val inPutFilePath: String =
                  "/media/gluk-alex/" +
                    "GDI/Java/Scala/sbt/projects/" +
                    "stanfordAlgorithmsDesignAndAnalysis1/" +
                    "src/test/scala/" +
                    "testTwoSumAlgorithm/"
                val inPutFileName: String =
                  "algo1-programming_prob-2sum.txt"
                val inputTakeNumber: Int = 10000
                /*lazy val (fileContentIter, fileContentIterCopy):
                (Iterator[String], Iterator[String]) =*/
                lazy val fileContentIter:
                Iterator[String] =
                //Iterator.empty
                  readFromFile(
                                fileName = inPutFileName,
                                filePath = inPutFilePath
                              )
                  /*reduce / control input size*/
                  .take(inputTakeNumber)
                  //.duplicate
                val heapSet: Stream[Long] =
                    fileContentIter
                      .map(_.toLong)
                    .toStream
                  //Stream(-5L, -4L, -3L, -2L, -1L, 0L, 1L, 2L, 3L, 4L, 5L)
                val minT: Long =
                  -10000L
                  //-2L
                val maxT: Long =
                    10000L
                  //2L

                //case class Sums(x: Long, y: Long, sum: Long)

                lazy val possibleCombinations:
                Stream[Stream[Long]] =
                  heapSet
                  .combinations(2)
                .toStream

                /*lazy*/ val startDateTime1:
                java.util.Date =
                  Calendar.getInstance().getTime()
                /*lazy*/ val startTimeStamp1: Long =
                  System.currentTimeMillis
                /*Date and Time Pattern */
                val timeStampFormat = new SimpleDateFormat("HH:mm:ss.SSS")
                lazy val startStampString1 =
                  timeStampFormat.format(startDateTime1)
                println(s"'possibleCombinations' started at:" +
                          startStampString1)
                val possibleSums:
                Stream[Long] =
                  possibleCombinations
                .view
                .collect{case Stream(x, y) if
                  x != y && x + y >= minT && x + y <= maxT =>
                    x + y}
                  .toStream
                lazy val endDateTime1 = Calendar.getInstance().getTime()
                lazy val endTimeStamp1: Long = System.currentTimeMillis()
                lazy val endStampString1 = timeStampFormat.format(endDateTime1)
                println(s"Done at:" + endStampString1)
                println(
                         s"time difference is:" +
                           (endTimeStamp1 - startTimeStamp1) + " Millis or:" +
                           convertLongToTimeString(
                                                    timeNumberMillis =
                                                      endTimeStamp1 -
                                                        startTimeStamp1,
                                                    colored = false
                                                  )
                       )

                /*println(
                         s"'possibleCombinations.size' is:" +
                          possibleCombinations.length
                       )
                println(
                         s"first '20' elements from " +
                           s"'possibleCombinations' are:\n" +
                          possibleCombinations
                          .take(20)
                          .map(_.mkString("[", ",", "]"))
                          /*.map(s=>
                                 s"[x:" + s.x +
                                   ",y:" + s.y +
                                   ",sum:" + s.sum + "]")*/
                          .mkString("{", ",", "}")
                )*/
                println(
                         s"`distinct` 'possibleSums.size' is:" +
                           possibleSums
                             .view
                           .distinct
                           .length
                       )
                /*println(s"first '20' elements from 'possibleSums' are:" +
                          possibleSums
                          .take(20)
                          /*.map(s=>
                                 s"[x:" + s.x +
                                   ",y:" + s.y +
                                   ",sum:" + s.sum + "]")*/
                          .mkString("{", ",", "}"))*/
                println(
                         s"first '20' `distinct` elements " +
                           s"from 'possibleSums' are:" +
                          possibleSums
                            .distinct
                          .take(20)
                          /*.map(s=>
                                 s"[x:" + s.x +
                                   ",y:" + s.y +
                                   ",sum:" + s.sum + "]")*/
                          .mkString("{", ",", "}"))

                assume(
                        possibleCombinations.nonEmpty &&
                          possibleSums.nonEmpty,
                        s"must be nonEmpty")

              }

}
