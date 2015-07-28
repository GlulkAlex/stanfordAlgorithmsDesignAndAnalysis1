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
      case (xyExtractor(x,y)) =>
        Some(ArbitraryPoint(x.toDouble, y.toDouble))
      case _ => None
    }

  def makePointsFromFileSource(source: Iterator[String]): Vector[ArbitraryPoint] = {
    def loop(extractedPoints: Vector[ArbitraryPoint]): Vector[ArbitraryPoint] = {
      if (source.hasNext) {
        val stringFromSource: String =
          source.next()
        val extractedPoint: Option[ArbitraryPoint] =
          extractCoordinatesFromSingleLine(expr=stringFromSource)
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
  /*Distances*/
}
