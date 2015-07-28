package testClosestPoints

import closestPoints.ClosestPoints.ArbitraryPoint
import closestPoints.ClosestPoints._
import filesIO.FilesIO._
import org.scalatest.FunSuite

/**
 * Created by gluk-alex on 7/23/15.
 */
class ClosestPointsSuit
  extends FunSuite {
  ignore(
        "1: 'makePointsFromFileSource'" +
          "should extract points from source"
      ) {
          val takeNumber: Int = 15
          val filePath: String =
            "/media/gluk-alex/" +
              "GDI/Java/Scala/sbt/projects/" +
              "stanfordAlgorithmsDesignAndAnalysis1/" +
              "src/test/scala/testClosestPoints/"
          val fileName: String = "JSON.txt"
          val actualFileContent: Iterator[String] =
            readFromFile(
                          fileName = fileName,
                          filePath = filePath
                        )
          val pointsVector: Vector[ArbitraryPoint] =
            makePointsFromFileSource(source = actualFileContent)

          println(
                   s"\nfirst $takeNumber lines from '$fileName' is:\n${
                     actualFileContent.take(10).mkString("\n")
                   }")
          println(
                   s"\nfirst $takeNumber points from '$fileName' is:\n${
                     pointsVector.take(10).mkString("\n")
                   }")
          assume(
                  //true == true,
                  pointsVector.length > 0,
                  "'pointsVector' must be non empty"
                )
        }
  ignore(
        "2: 'pointsDistancesCombinator'" +
          "should eval all possible points distances"
      ) {
          val takeNumber: Int = 15
          val filePath: String =
            "/media/gluk-alex/" +
              "GDI/Java/Scala/sbt/projects/" +
              "stanfordAlgorithmsDesignAndAnalysis1/" +
              "src/test/scala/testClosestPoints/"
          val fileName: String = "JSON.txt"
          val actualFileContent: Iterator[String] =
            readFromFile(
                          fileName = fileName,
                          filePath = filePath
                        )
          val pointsVector: Vector[ArbitraryPoint] =
            makePointsFromFileSource(source = actualFileContent)
          val pointsDistances: Vector[PairOfPointsResult] =
            pointsDistancesCombinator(pointsVector)

          println(
                   s"\nfirst $takeNumber lines from '$fileName' is:\n${
                     actualFileContent.take(10).mkString("\n")
                   }")
          println(
                   s"\nfirst $takeNumber distances from 'pointsDistances' is:\n${
                     pointsDistances.take(10).mkString("\n")
                   }")
          /*println(
                   s"\nfirst $takeNumber points from '$fileName' is:\n${
                     pointsVector.take(10).mkString("\n")
                   }")*/
          assume(
                  //true == true,
                  pointsDistances.length > 0,
                  "'pointsDistances' must be non empty"
                )
        }
  test(
        "3: 'pointsDistancesCombinator'" +
          "should contain minimum points distances"
      ) {
          val takeNumber: Int = 15
          val filePath: String =
            "/media/gluk-alex/" +
              "GDI/Java/Scala/sbt/projects/" +
              "stanfordAlgorithmsDesignAndAnalysis1/" +
              "src/test/scala/testClosestPoints/"
          val fileName: String = "JSON.txt"
          val actualFileContent: Iterator[String] =
            readFromFile(
                          fileName = fileName,
                          filePath = filePath
                        )
          val pointsVector: Vector[ArbitraryPoint] =
            makePointsFromFileSource(source = actualFileContent)
          val pointsDistances: Vector[PairOfPointsResult] =
            pointsDistancesCombinator(pointsVector)
          val minDistance: Double =
            pointsDistances
            .minBy((x)=>x.distance).distance

          println(
                   s"\nfirst $takeNumber lines from '$fileName' is:\n${
                     actualFileContent.take(10).mkString("\n")
                   }")
          println(
                   s"\nfirst $takeNumber distances from 'pointsDistances' is:\n${
                     pointsDistances.take(10).mkString("\n")
                   }")
          println(
                   s"\n'minDistance' from 'pointsDistances' is:\n${
                     minDistance
                   }")
          /*println(
                   s"\nfirst $takeNumber points from '$fileName' is:\n${
                     pointsVector.take(10).mkString("\n")
                   }")*/
          assume(
                  //true == true,
                  minDistance >= 0.0,
                  "'pointsDistances' must be non empty"
                )
        }
}
