package filesIO

import scala.io._
import scala.io.Source
import scala.collection.mutable.Buffer
import scala.util.{Try, Success, Failure}
import java.io._
import java.io.PrintWriter
import java.io.FileReader
import java.io.FileNotFoundException
import java.io.IOException

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
    /*
    'PrintWriter(String fileName, String csn)'
    Creates
    a new 'PrintWriter',
    without automatic `line flushing`,
    with the specified `file name` and `charset`.
     */
      new PrintWriter(filePath + fileName)
    //var lastLine: String = ""
    var isFirstLine: Boolean = true

    adjacencyList
    .foreach({ case (key, adjustedSet) => //{
      /*side effect*/
      val lastLine =
        if (isFirstLine) {
          isFirstLine = false

          key + "," + adjustedSet.mkString(",") // + "EOL"
        } else {
          if (adjustedSet.nonEmpty) {
            "\n" + key + "," + adjustedSet.mkString(",") // + "EOL"
          } else {
            "\n" + key
          }
        }
      /*
      Prints a String and then terminates the line.
      println(String x)
       */
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

  def writeAdjustedListToTextFileByChunk(
                                          filePath: String =
                                          "/home/gluk-alex/Documents/",
                                          fileName: String =
                                          "graphPostOrder.txt",
                                          //"diGraphMapReversed.txt",
                                          adjacencyList:
                                          Map[Int, Set[Int]] =
                                          Map.empty,
                                          chunkSize: Int = 1
                                          ) {
    val mapIter: Iterator[Map[Int, Set[Int]]] =
      adjacencyList
      .sliding(chunkSize, chunkSize)
    //val linesChunk: String = ""
    //var isFirstLine: Boolean = true
    var isFirstChunk: Boolean = true

    mapIter
    .foreach(chunk => {
      val linesChunk: String =
        chunk
        .map({ case (key, adjustedSet) => //{
          /*side effect*/
          //val nextLine: String =
            if (adjustedSet.nonEmpty) {
              key + "," + adjustedSet.mkString(",") // + "\n"
            } else {
              key + ""
            }
               //}
             }
            ).mkString("\n")
      /*side effect*/
      if (isFirstChunk) {
        //append = false
        isFirstChunk = false
      } else {
        //append = true

      }
      appendBatchOfDataToExistingFile(
                                       filePath =
        "/home/gluk-alex/Documents/",
      fileName =
      "graphPostOrder.txt",
      //"diGraphMapReversed.txt",
      linesChunk = linesChunk,
                                       append = true
      )
    }
            )
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

  def appendBatchOfDataToExistingFile(
                                       filePath: String =
                                       "/home/gluk-alex/Documents/",
                                       fileName: String =
                                       "graphPostOrder.txt",
                                       //"diGraphMapReversed.txt",
                                       /*adjacencyListBatched:
                                       Iterator[Map[Int, Set[Int]]] =
                                       Iterator.empty*/
                                       /*adjacencyListSlide:
                                     Map[Int, Set[Int]] =
                                     Map.empty*/
                                       linesChunk: String,
                                       append: Boolean = true
                                       ): Unit = {
    //val buf = new StringBuilder
    //val buf: Buffer[String] = Buffer.empty

    //buf.appendAll(('A' to 'Z').map(_.toString))
    //('A' to 'Z').toBuffer

    /*val slideContent: String =
      adjacencyListSlide
        .view
      .map({ case (key, adjustedSet) =>
            if (adjustedSet.nonEmpty) {
              key + "," + adjustedSet.mkString(",")  + "\n"
            } else {
              key + "\n"
            }
          })
        /*to drop last "\n"*/
        .init
    .toString()*/

    val fw: FileWriter =
      new FileWriter(
                      /*param: 'fileName' - String The system-dependent
                      filename*/
                      filePath + fileName,
                      /*'append' - boolean
                      if true, then
                      data will be written to the `end` of the file
                      rather than the `beginning`.
                      */
                      append
                      //true
                    )

    try {
      fw
      .write(
          //slideContent
          if (append) {
            /*not continue existing line, but start from new*/
            "\n" + linesChunk
          } else {
            linesChunk
          }
            )
    }
    finally fw.close()
  }

  /*from
  http://stackoverflow.com/questions/6870145/how-do-i-append-to-a-file-in-scala
   */
  def appendToExistingFile {
    /*The Java FileWriter constructor is called like this:
    'new FileWriter(String s, boolean append)'
    This simple constructor indicates that
    you want to write to the file in `append` `mode`.
    */
    val fw: FileWriter =
      new FileWriter(
                      "test.txt",
                      true
                    )

    //fw.write("This line appended to file!")
    try {
      fw
      .write(/* your stuff */ "my file content")
    }
    finally fw.close()
  }

  /*from
  http://alvinalexander.com/java/edu/qanda/pjqa00009.shtml
   */
  /*
  Suppose checkbook.dat currently contains these two entries:
  >398:08291998:Joe's Car Shop:101.00
  >399:08301998:Papa John's Pizza:16.50
   */
  def appendToExistingFile2: Unit = {
    var bw: BufferedWriter = null

    try {
      // APPEND MODE SET HERE
      bw = new BufferedWriter(new FileWriter("checkbook.dat", true))
      bw
      .write("400:08311998:Inprise Corporation:249.95")
      bw
      .newLine()
      bw
      .flush()
    } catch {
      case ex: FileNotFoundException => {
        println("Missing file exception")
      }
      case ex: IOException           => {
        println("IO Exception")
        ex
        .printStackTrace()
      }
    } finally {
      // always close the file
      if (bw != null) {
        try {
          bw.close()
        } catch {
          // just ignore it
          case ex: IOException => {
            println("IO Exception")
          }
        }
      }
    } // end try/catch/finally
  }

}
