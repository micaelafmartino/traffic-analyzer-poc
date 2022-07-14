package co.topl.traffic

import co.topl.traffic.analyzer.TrafficAnalyzer
import co.topl.traffic.analyzer.TrafficAnalyzer.LeadingSegments
import co.topl.traffic.model.cityNetwork.{Intersection, RoadSegment}
import co.topl.traffic.model.json.{TrafficMeasurement, TrafficMeasurements}
import co.topl.traffic.utils.extensions.JsValueExtensions

//import zio.Console._
//import zio._

//object Main extends ZIOAppDefault {
//  lazy val run = for {
//    _ <- printLine("Hello! What is your name?")
//    name <- readLine
//    _ <- printLine(s"Hello, $name, welcome to ZIO!")
//  } yield ()
//}

object Main extends App {
  val url = """file:src/dist/sample-data.json"""
  val measurements: List[TrafficMeasurement] = TrafficMeasurements.readMeasurements(url).get.trafficMeasurements.head.measurements
  val json = TrafficMeasurements.readMeasurementsJson(url).get.prettyString()
  val adjacencies: Map[Intersection, List[RoadSegment]] = measurements.map(_.segment).groupBy(_.start)

  val intersections: Set[Intersection] = measurements.map(_.segment).flatMap(m => List(m.start, m.end)).toSet
  val leadingSegments: LeadingSegments = measurements.map(_.segment).groupBy(_.end)

  println(TrafficAnalyzer(leadingSegments, intersections.last).shortestPathFrom(intersections.head))
  println()
  TrafficAnalyzer(leadingSegments, intersections.last).shortestPaths().pathToTarget.foreach(println)

  println()
  println("total intersections: " + intersections.size)
  println("with 3 or more possible paths: " + adjacencies.count(_._2.size > 2))
  println("with only 1 possible path: " + adjacencies.count(_._2.size == 1))
  println("with 2 possible paths: " + adjacencies.count(_._2.size == 2))
  //  adjacencies.mapValues(_.size).filter(_._2 < -1).foreach(println)
}