package closestPoints

//import scala.util.parsing.json

import scala.math._
import scala.util.matching.Regex

/**
 * Created by gluk-alex on 7/25/15.
 */
object ClosestPoints {

  case class ArbitraryPoint(
                             x: Double,
                             y: Double
                             )

  case class PairOfPointsResult(
                                 point1: ArbitraryPoint,
                                 point2: ArbitraryPoint,
                                 /*x1: Double,
                                 y1: Double,
                                 x2: Double,
                                 y2: Double,*/
                                 distance: Double
                                 )

  /*extractors regEx groups*/
  val xyExtractor: Regex =
    """\D+(\d+\.\d{2})\D+(\d+\.\d{2}).+""".r

  def extractCoordinatesFromSingleLine(expr: String): Option[ArbitraryPoint] =
    expr match {
      case (xyExtractor(x, y)) => {
        {
          Some(ArbitraryPoint(x.toDouble, y.toDouble))
        }
      }
      case _                   => None
    }

  def makePointsFromFileSource(source: Iterator[String]):
  Vector[ArbitraryPoint] = {
    def loop(extractedPoints: Vector[ArbitraryPoint]): Vector[ArbitraryPoint]
    = {
      if (source.hasNext) {
        val stringFromSource: String =
          source.next()
        val extractedPoint: Option[ArbitraryPoint] =
          extractCoordinatesFromSingleLine(expr = stringFromSource)
        /*recursion*/
        if (extractedPoint.isEmpty) {
          /*same as before*/
          loop(extractedPoints)
        } else {
          /*new point added*/
          loop(extractedPoint.get +: extractedPoints)
        }
      } else {
        /*return value*/
        extractedPoints
      }
    }
    /*initialization*/
    /*return value*/
    loop(Vector.empty)
  }

  def evalDistance(
                    point1: ArbitraryPoint,
                    point2: ArbitraryPoint): Double = {
    /*return value*/
    sqrt(pow(point2.x - point1.x, 2) + pow(point2.y - point1.y, 2))
    //0.0
  }

  //package scala.util.parsing.json
  /*return closest pair of points*/
  def straitForwardClosest(
                            pointSeq: Vector[(Double, Double)],
                            leftToCompareSeq: Vector[(Double, Double)],
                            closestPair: PairOfPointsResult
                            ): PairOfPointsResult = {
    //(Double, Double)] = {
    /*
    for each point must calculate:
    distance to all other points &
    remember smallest
     */
    def innerLoop(
                   innerLeftToCompareSeq: Vector[(Double, Double)],
                   innerClosestPair: PairOfPointsResult
                   ): PairOfPointsResult = {
      if (innerLeftToCompareSeq.isEmpty) {
        /*return value*/
        innerClosestPair
      } else {
        /*eval distance*/
        /*recursion*/
        innerLoop(
                   innerLeftToCompareSeq.tail,
                   innerClosestPair: PairOfPointsResult
                 )
      }
    }

    if (leftToCompareSeq.isEmpty) {
      closestPair
    } else {
      /*recursion*/
      straitForwardClosest(
                            pointSeq: Vector[(Double, Double)],
                            leftToCompareSeq.tail,
                            closestPair: PairOfPointsResult
                          )
    }
    /*initialization*/
    innerLoop(
               leftToCompareSeq,
               PairOfPointsResult(
                                   /*x1=Double.PositiveInfinity,
                                   y1=Double.PositiveInfinity,
                                   x2=Double.PositiveInfinity,
                                   y2=Double.PositiveInfinity,*/
                                   ArbitraryPoint(Double
                                                  .PositiveInfinity,
                                                  Double
                                                  .PositiveInfinity),
                                   ArbitraryPoint(Double
                                                  .PositiveInfinity,
                                                  Double
                                                  .PositiveInfinity),
                                   distance = Double.PositiveInfinity
                                 )
             )
    /*return value*/
    //Vector((0.0, 0.0), (0.0, 0.0))
    PairOfPointsResult(
                        /*x1=0.0,
                        y1=0.0,
                        x2=0.0,
                        y2=0.0,*/
                        ArbitraryPoint(
                                        Double.PositiveInfinity,
                                        Double.PositiveInfinity),
                        ArbitraryPoint(
                                        Double.PositiveInfinity,
                                        Double.PositiveInfinity),
                        distance = 0.0
                      )
  }

  /*calculate twice when switch start & end point in pair*/
  def pointsDistancesCombinator(pointsSeq: Vector[ArbitraryPoint]):
  Vector[PairOfPointsResult] = {
    for {
      p1 <- pointsSeq
      p2 <- pointsSeq if p1 != p2
    } yield PairOfPointsResult(
                                ArbitraryPoint(
                                                p1.x,
                                                p1.y),
                                ArbitraryPoint(
                                                p2.x,
                                                p2.y),
                                distance =
                                  evalDistance(p1, p2)
                              )
  }

  def pointsDistancesCombinatorImproved(pointsSeq: Vector[ArbitraryPoint]):
  Vector[PairOfPointsResult] = {
    (for {
      i <- pointsSeq.indices
      //j <- i + 1 to pointsSeq.indices.last
      j <- pointsSeq.indices if j > i
    } yield {
        val p1 = pointsSeq(i)
        val p2 = pointsSeq(j)
        /*return value*/
        PairOfPointsResult(
                            ArbitraryPoint(
                                            p1.x,
                                            p1.y),
                            ArbitraryPoint(
                                            p2.x,
                                            p2.y),
                            distance =
                              evalDistance(p1, p2)
                          )
      })
    .toVector
  }

  /*
  * ? may be it is make sense to
  * to choose what coordinate
  * 'x' or 'y'
  * make biggest impact / input / influence on distance
  * by comparing 1-st two element difference
  * in sorted by 'x', 'y' points ?
  * 1>sort 'pointsSeq' by 'x'
  * 2>find median by 'x'
  * 3>partition by median to left, pivot, right part
  * (? is `median` value excluded from parts ?)
  * base cases:
   * if 'pointsSeq.size == 1'
   * then
   * 'delta' = PositiveInfinity
   * if 'pointsSeq.size == 2'
   * then
   * 'delta' = abs(point2.x - point1.x)
   *else
   * recursion:
   * 'leftDelta' = closestPair(leftPart)
   * 'rightDelta' = closestPair(rightPart)
   * minimum distance across the cut (for 1-D only)
   * {
   * 'medianDelta' = ? min(median - leftPart.last.x, rightPart.head.x - median)
   * return value
   * min('leftDelta', 'medianDelta', 'rightDelta')
   * }
   * for 2-D:
   * 'delta' = min('leftDelta', 'rightDelta')
   * 4>closest pair for points with
   * 'x' == 'median' or
   * 'x' <= 'median' + delta
   * 'x' <= 'median' - delta
  * */
  def closestPair(
                   pointsSeq: Vector[ArbitraryPoint],
                   pointsSortedByX: Vector[ArbitraryPoint] = Vector.empty
                   ): Option[PairOfPointsResult] = {
    val sortedXs: Vector[ArbitraryPoint] =
      if (pointsSortedByX.isEmpty &&
        pointsSeq.nonEmpty &&
        pointsSeq.length > 2
      ) {
        pointsSeq
        .sortBy((x) => x.x)
      } else {
        pointsSeq
      }
    /*return value*/
    Some(
          PairOfPointsResult(
                              ArbitraryPoint(
                                              0.0,
                                              0.0),
                              ArbitraryPoint(
                                              0.0,
                                              0.0),
                              distance =
                                Double.PositiveInfinity
                            )
        )
    None
  }

  /*Striped*/
  def findSripedPoints(
                        pointsSeq: Vector[ArbitraryPoint],
                        stripeCenterX: Double
                        ): Vector[ArbitraryPoint] = {
    /*return value*/
    pointsSeq
  }

  def closestSplitPair(
                        pointsSeq: Vector[ArbitraryPoint],
                        delta: Double) {
    /*? median ?*/
    val xClosestToMidFromLeft: Double = 0.0
    /*within 'xClosestToMidFromLeft - / + delta'*/
    val stripedPoints: Vector[ArbitraryPoint] =
      findSripedPoints(
                        pointsSeq: Vector[ArbitraryPoint],
                        stripeCenterX = xClosestToMidFromLeft
                      )
    //var deltaCurrentBest: Double = delta
    /*??? may be 'distance' + infinity */
    var bestPair: Option[PairOfPointsResult] = None

    /*very strange iteration counter / range here*/
    for {
      i <- stripedPoints.indices.dropRight(7)
      //i<- 1 to stripedPoints.length -7
      j <- 1 to 7
    } yield {
      val checkPointsDistance: PairOfPointsResult =
        PairOfPointsResult(
                            stripedPoints(i),
                            stripedPoints(i + j),
                            evalDistance(stripedPoints(i),
                                         stripedPoints(i + j)))
      /*return value*/
      //if (checkPointsDistance.distance<deltaCurrentBest) {
      if (checkPointsDistance.distance < bestPair.get.distance) {
        bestPair = Some(checkPointsDistance)
      }

    }

  }
}
