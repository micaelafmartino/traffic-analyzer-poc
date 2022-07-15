package co.topl.traffic.utils

import co.topl.traffic.model.cityNetwork.{Intersection, RoadSegment}
import co.topl.traffic.parser.TrafficMeasurementsParser

/** DOT file generator to plot the graph using Graphviz */
object GraphvizGenerator extends App {
  val url = """file:src/dist/sample-data.json"""
  val segments: Set[RoadSegment] = TrafficMeasurementsParser(url).roadSegments().get

  val intersectionsMatrix: List[List[Intersection]] = segments
    .flatMap(m => List(m.start, m.end))
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
