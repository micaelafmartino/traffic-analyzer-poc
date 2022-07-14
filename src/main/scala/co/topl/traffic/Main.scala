package co.topl.traffic

import co.topl.traffic.analyzer.TrafficAnalyzer
import co.topl.traffic.model.cityNetwork.Intersection
import co.topl.traffic.model.json.{BestPath, TrafficMeasurement}
import co.topl.traffic.parser.TrafficMeasurementsParser
import play.api.libs.json.Json
import zio.Console._
import zio._

import java.net.URL
import scala.util.Try

object Main extends ZIOAppDefault {
  private val intersectionParser = """([a-zA-Z]+)(\d+)""".r

  lazy val run = for {
    _ <- printLine("Enter the path to the measurements file (URL format):")
    url <- readLine
    _ <- printLine("Enter the source intersection (in the format <avenue><street>, e.g. A1):")
    source <- readLine
    _ <- printLine("Enter the target intersection (in the format <avenue><street>, e.g. F10):")
    target <- readLine
    _ <- printLine(Json.toJson(getBestRoute(url, source, target).get).toString())
  } yield ()

  private def getBestRoute(url: String, source: String, target: String): Try[BestPath] = for {
    (url, source, target) <- Try {
      val intersectionParser(sourceAvenue, sourceStreet) = source
      val intersectionParser(targetAvenue, targetStreet) = target
      new URL(url) // url validation
      (url, Intersection(avenue = sourceAvenue, street = sourceStreet), Intersection(avenue = targetAvenue, street = targetStreet))
    }
    allMeasurements <- TrafficMeasurementsParser(url).readMeasurements()
    averagedSegments = allMeasurements
      .trafficMeasurements
      .flatMap(_.measurements)
      .groupBy(_.segment)
      .map { case (segment, measurements) => segment.copy(transitTime = avgTransitTime(measurements)) }
      .toList
    (totalTransitTime, path) = TrafficAnalyzer.from(averagedSegments, target).shortestPathFrom(source)
  } yield BestPath(source = source.toString, target = target.toString, totalTransitTime = totalTransitTime, path = path.dropRight(1).map(_.toString))

  private def avgTransitTime(measurements: List[TrafficMeasurement]): Double = measurements.map(_.transitTime).sum / measurements.size.toDouble
}