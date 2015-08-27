package filesIO

import scala.io._
import scala.io.Source
import scala.collection.mutable.Buffer
import java.io._
import java.io.PrintWriter

/**
 * Created by gluk-alex on 7/25/15.
 */
object FilesIO {
  /*
            Calling 'head' on a `buffered` `iterator`
            will return
            its first `element` but
            will not `advance` the `iterator`.
             */
  /*val bit: BufferedIterator[String] =
    fileContentIter
    .buffered
  def skipEmptyWords(it: BufferedIterator[String]) =
    while (it.head.isEmpty) {it.next()}
    */

  /*if path & name OK this is enough*/
  def readFromFile(
                    fileName: String =
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
      .fromFile(filePath + fileName)
      .getLines()
    /*return value*/
    currFileLines
  }

  def writeToTextFile(
                       filePath: String = "",
                       fileName: String = "test.txt"
                       ) {
    val writer =
      new PrintWriter(new File(fileName))

    writer.write("Hello Scala")
    writer.close()
  }

  def writeToTextFile2(
                        filePath: String = "",
                        fileName: String = "test2.txt"
                        ) {
    val writer =
      new PrintWriter(fileName)

    for (i <- 1 to 10) {
      writer.print(i)
      writer.print(" --> ")
      writer.println(i * i)
    }

    writer.close()
  }

  def writeToTextFile3(
                        filePath: String = "",
                        fileName: String = "test3.txt"
                        ) {
    val writer =
      new PrintWriter(fileName)

    for (i <- 1 to 10) {
      writer.print("%3d --> %d\n".format(i, i * i))
    }

    writer.close()
  }

  /*
  * JSON uses JavaScript syntax,
  * but the JSON format is text only, just like XML
  JSON Example:
    {"employees":[
        {"firstName":"John", "lastName":"Doe"},
        {"firstName":"Anna", "lastName":"Smith"},
        {"firstName":"Peter", "lastName":"Jones"}
    ]}
   */
  def writeJSON_ToFile(
                        filePath: String = "",
                        fileName: String = "JSON.txt",
                        fileContent: Seq[(Double, Double)] //String = ""
                        ) {
    val writer =
      new PrintWriter(filePath + fileName)

    writer
    //println(f"$name%s is $height%2.2f meters tall")
    .print( s"""{\"coordinates\":[\n""")

    for (elem <- fileContent) {
      val lastElem = fileContent.last

      if (elem == lastElem) {
        writer
        .print( s"""{\"x\":\"${elem._1}\", \"y\":\"${elem._2}\"}\n""")
      } else {
        writer
        //.print("%3d --> %d\n".format(i, i*i))
        .print( s"""{\"x\":\"${elem._1}\", \"y\":\"${elem._2}\"},\n""")
      }
    }

    writer
    .print(s"]}")

    writer.close()
  }

  def writeAdjustedListToTextFile(
                                   filePath: String =
                                   "/home/gluk-alex/Documents/",
                                   fileName: String =
                                   "diGraphMapReversed.txt",
                                   adjacencyList:
                                   Map[Int, Set[Int]] =
                                   Map.empty
                                   ) {
    val writer =
      new PrintWriter(filePath + fileName)
    //var lastLine: String = ""
    var isFirstLine: Boolean = true

    adjacencyList
    .foreach({ case (key, adjustedSet) => //{
      /*side effect*/
      val lastLine =
        if (isFirstLine) {
          isFirstLine = false

          key + "," + adjustedSet.mkString(",")// + "EOL"
        } else {
          "\n" + key + "," + adjustedSet.mkString(",")// + "EOL"
        }

      writer
      .print(lastLine)
    //}
             }
            )
    //writer
    //.print(lastLine.dropRight(4))
    writer
    .close()
  }

  def writeStreamToTextFile(
                                   filePath: String =
                                   "/home/gluk-alex/Documents/",
                                   fileName: String =
                                   "graphPostOrder.txt",
                                   sourceStream:
                                   Stream[Int] =
                                   Stream.empty
                                   ) {
    val writer =
      new PrintWriter(filePath + fileName)
    var isFirstLine: Boolean = true

    sourceStream
    .foreach({ case digit => //{
      /*side effect*/
      val nextLine =
        if (isFirstLine) {
          isFirstLine = false

          digit
        } else {
          "\n" + digit
        }

      writer
      .print(nextLine)
    //}
             }
            )

    writer
    .close()
  }

  @scala.annotation.tailrec
  def extractDigitsFromString(
                               sourceStr: String,
                               digits:
                               //List[String] =
                               List[Int] =
                               List.empty,
                               /*Buffer[Int] =
                               Buffer.empty,*/
                               accum: String = ""
                               ):
  //List[String] = {
  //Buffer[Int] = {
  List[Int] = {
    if (sourceStr.isEmpty) {
      accum match {
        case "" => digits
        case a  => digits :+ a.toInt
        //.reverse
      }
    } else {
      val (digitsUpdated, accumUpdated):
      //(Buffer[Int], String) =
      (List[Int], String) =
        if (sourceStr.head.isDigit) {
          (digits, accum + sourceStr.head)
        } else /*if (sourceStr.head.isSpaceChar)*/ {
          if (
            accum.isEmpty
          ) {
            (digits, accum)
          } else {
            //(digits += accum.toInt, "")
            (digits :+ accum.toInt, "")
            //(accum.toInt +: digits, "")
          }
        }
      /*recursion*/
      extractDigitsFromString(
                               sourceStr = sourceStr.tail,
                               digits = digitsUpdated,
                               accum = accumUpdated
                             )
    }
  }

}
