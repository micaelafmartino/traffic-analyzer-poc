package co.topl.traffic

import co.topl.traffic.analyzer.TrafficAnalyzer
import co.topl.traffic.errors._
import co.topl.traffic.model.cityNetwork.{Intersection, RoadSegment}
import co.topl.traffic.model.json.BestRoute
import co.topl.traffic.parser.TrafficMeasurementsParser
import play.api.libs.json.Json
import zio.Console._
import zio._

import java.io.IOException
import java.net.URL
import scala.util.Try

object Main extends ZIOAppDefault {
  private val defaultSampleFileUrl = "file:src/dist/sample-data.json"

  def run = shellProgram
    .repeatUntilEquals("q")
    .catchSome {
      case managedError: ManagedError => managedErrorsHandler(managedError)
      case e => printLine(s"Unexpected error ocurred: $e")
    }

  lazy val shellProgram: Task[String] = for {
    _ <- printLine("""Enter a valid URL to the measurements file (press enter for default file):""")
    url <- retryableInput(readLine.flatMap(validateInputURL))
    _ <- printLine(s"Using $url file")

    _ <- printLine("Enter the source intersection (in the format <avenue><street>, e.g. A1):")
    source <- retryableInput(readLine.flatMap(parseInputIntersection))

    _ <- printLine("Enter the target intersection (in the format <avenue><street>, e.g. F10):")
    target <- retryableInput(readLine.flatMap(parseInputIntersection))

    averagedSegments <- parseRoadSegments(url)
    analyzer <- ZIO.fromTry(TrafficAnalyzer.from(averagedSegments, target))
    bestRoute <- ZIO.fromOption(findBestRoute(analyzer, source)).mapError(_ => NoRouteFound(source = source, target = target))

    _ <- printLine(s"The best route from $source to $target is:")
    _ <- printLine(Json.toJson(bestRoute).toString)

    _ <- printLine("")
    _ <- printLine("""Press enter to start again or type "q" to quit""")
    quitOrRepeat <- readLine
  } yield quitOrRepeat

  def parseInputIntersection(stringIntersection: String): IO[UserInputParseError, Intersection] = ZIO.fromTry(Try {
    val Intersection.parser(sourceAvenue, sourceStreet) = stringIntersection
    Intersection(avenue = sourceAvenue, street = sourceStreet)
  }).mapError(UserInputParseError("intersection", stringIntersection, _))

  def validateInputURL(url: String): IO[UserInputParseError, String] = {
    if (url.isBlank) ZIO.succeed(defaultSampleFileUrl)
    else ZIO.fromTry(Try(new URL(url).toString)).mapError(UserInputParseError("url", url, _))
  }

  def parseRoadSegments(url: String): IO[TrafficDataParseError, Set[RoadSegment]] = ZIO
    .attemptBlocking(TrafficMeasurementsParser(url).roadSegments())
    .flatMap(ZIO.fromTry(_))
    .mapError(TrafficDataParseError)

  def findBestRoute(analyzer: TrafficAnalyzer, source: Intersection): Option[BestRoute] = analyzer.shortestPathFrom(source).map { case (totalTransitTime, path) =>
    BestRoute(
      source = source.toString,
      target = analyzer.target.toString,
      totalTransitTime = totalTransitTime,
      path = path.dropRight(1).map(_.toStringWithoutTime) // the last segment is the self path from target to target with 0 transit time
    )
  }

  def managedErrorsHandler(e: ManagedError): IO[IOException, Unit] = e match {
    case UserInputParseError(inputName, input, _) => printLine(s"Invalid $inputName: $input")
    case TrafficDataParseError(_) => printLine("Couldn't find a valid traffic measurements file at the given URL")
    case NoRouteFound(source, target) => printLine(s"Couldn't find any route between source $source and target $target")
    case TrafficAnalyzerInitError(detail, _) => printLine(s"Internal analyzer error: $detail")
  }

  private def retryableInput[A](io: Task[A], retries: Int = 3, message: String = "Invalid input, try again:"): Task[A] = io.orElse(
    printLine(message)
      .flatMap(_ => io)
      .retryN(retries - 1) // first is the orElse recover, then n-1 retries
  ).onError(_ => printLine("Too many retries..").catchAll(_ => ZIO succeed())) // ignore exit printLine errors
}