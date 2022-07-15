package co.topl.traffic.model

package object cityNetwork {
  /** Intersection between and Avenue and a Street (in a grid pattern) */
  case class Intersection(avenue: String, street: String) {
    override def toString: String = s"$avenue$street"
  }

  object Intersection {
    // streets should be always numbers, so for now no reason to uppercase them
    def apply(avenue: String, street: String): Intersection = new Intersection(avenue.toUpperCase, street)
  }

  /** One-way segment of the road bewteen two intersections with the time it takes to traverse it */
  case class RoadSegment(transitTime: Double, start: Intersection, end: Intersection) {
    override def toString: String = s"$start -> $end"
  }
}
