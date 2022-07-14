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
  val segments = measurements.map(_.segment)
  val json = TrafficMeasurements.readMeasurementsJson(url).get.prettyString()
  val adjacencies: Map[Intersection, List[RoadSegment]] = segments.groupBy(_.start)

  val intersections: Set[Intersection] = segments.flatMap(m => List(m.start, m.end)).toSet
  val leadingSegments: LeadingSegments = segments.groupBy(_.end)

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

/** DOT file generator to plot the graph using Graphviz */
object GraphvizGenerator extends App {
  val url = """file:src/dist/sample-data.json"""
  val segments: List[RoadSegment] = TrafficMeasurements
    .readMeasurements(url).get
    .trafficMeasurements.head
    .measurements
    .map(_.segment)

  val intersections = segments.flatMap(m => List(m.start, m.end)).toSet

  val intersectionsMatrix: List[List[Intersection]] = intersections
    .groupBy(_.street.toInt)
    .view.mapValues(_.toList.sortBy(_.avenue))
    .toList.sortBy(_._1)
    .map(_._2)

  val horizontalGrid = intersectionsMatrix
    .map(_.mkString("rank=same {", " -> ", "}"))
    .mkString("\n")

  val verticalGrid = intersectionsMatrix
    .transpose
    .map(_.mkString(" -> "))
    .mkString("\n")

  val edges: String = segments
    .map(s => s""""${s.start}" -> "${s.end}" [label = "${s.transitTime.round}"]""")
    .mkString("\n")

  val graphvizCode =
    s"""
       |digraph {
       |node [shape = plaintext]
       |splines = false
       |
       |$edges
       |
       |edge [style=invis]
       |
       |$verticalGrid
       |
       |$horizontalGrid
       |}
       |""".stripMargin

  println(graphvizCode)
}