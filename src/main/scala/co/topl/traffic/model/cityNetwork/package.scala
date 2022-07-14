package co.topl.traffic.model

package object cityNetwork {
  /** Intersection between and Avenue and a Street (in a grid pattern) */
  case class Intersection(street: String, avenue: String)

  /** One-way segment of the road bewteen two intersections with the time it takes to traverse it */
  case class RoadSegment(transitTime: Double, start: Intersection, end: Intersection)
}