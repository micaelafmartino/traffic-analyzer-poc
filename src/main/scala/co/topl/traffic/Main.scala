package co.topl.traffic

import co.topl.traffic.analyzer.TrafficAnalyzer
import co.topl.traffic.errors.{NoRouteFound, TrafficAnalyzerInitError, TrafficDataParseError, UserInputParseError}
import co.topl.traffic.model.cityNetwork.{Intersection, RoadSegment}
import co.topl.traffic.model.json.BestRoute
import co.topl.traffic.parser.TrafficMeasurementsParser
import play.api.libs.json.Json
import zio.Console._
import zio._

import java.net.URL
import scala.util.Try

object Main extends ZIOAppDefault {
  private val intersectionParser = """([a-zA-Z]+)(\d+)""".r

  def run = shellProgram.catchSome {
    case UserInputParseError(inputName, input, _) => printLine(s"Invalid $inputName: $input")
    case TrafficDataParseError(_) => printLine("Couldn't find a valid traffic measurements file at the given URL")
    case NoRouteFound(source, target) => printLine(s"Couldn't find any route between source $source and target $target")
    case TrafficAnalyzerInitError(detail, _) => printLine(s"Internal analyzer error: $detail")
    case e => printLine(s"Unexpected error ocurred: $e")
  }

  lazy val shellProgram = for {
    _ <- printLine("Enter the path to the measurements file (URL format):")
    url <- readLine.flatMap(validateInputURL)

    _ <- printLine("Enter the source intersection (in the format <avenue><street>, e.g. A1):")
    source <- readLine.flatMap(parseInputIntersection)

    _ <- printLine("Enter the target intersection (in the format <avenue><street>, e.g. F10):")
    target <- readLine.flatMap(parseInputIntersection)

    averagedSegments <- parseRoadSegments(url)
    analyzer <- ZIO.fromTry(TrafficAnalyzer.from(averagedSegments, target))
    bestRoute <- ZIO.fromOption(findBestRoute(analyzer, source)).mapError(_ => NoRouteFound(source = source, target = target))

    _ <- printLine(s"The best route from $source to $target is:")
    _ <- printLine(Json.toJson(bestRoute).toString)
  } yield ()

  def parseInputIntersection(stringIntersection: String): IO[UserInputParseError, Intersection] = ZIO.fromTry(Try {
    val intersectionParser(sourceAvenue, sourceStreet) = stringIntersection
    Intersection(avenue = sourceAvenue, street = sourceStreet)
  }).mapError(UserInputParseError("intersection", stringIntersection, _))

  def validateInputURL(url: String): IO[UserInputParseError, String] = ZIO.fromTry(Try(new URL(url).toString)).mapError(UserInputParseError("url", url, _))

  def parseRoadSegments(url: String): IO[TrafficDataParseError, List[RoadSegment]] = ZIO
    .attemptBlocking(TrafficMeasurementsParser(url).roadSegments())
    .flatMap(ZIO.fromTry(_))
    .mapError(TrafficDataParseError)

  def findBestRoute(analyzer: TrafficAnalyzer, source: Intersection): Option[BestRoute] = analyzer.shortestPathFrom(source).map { case (totalTransitTime, path) =>
    BestRoute(
      source = source.toString,
      target = analyzer.target.toString,
      totalTransitTime = totalTransitTime,
      path = path.dropRight(1).map(_.toString) // the last segment is the self path from target to target with 0 transit time
    )
  }
}