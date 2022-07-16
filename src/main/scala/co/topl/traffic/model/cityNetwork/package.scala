package co.topl.traffic.model

package object cityNetwork {
  /** Intersection between and Avenue and a Street (in a grid pattern) */
  case class Intersection(avenue: String, street: String) {
    override def toString: String = s"$avenue$street"
  }

  object Intersection {
    val parser = """([a-zA-Z]+)(\d+)""".r

    // streets should be always numbers, so for now no reason to uppercase them
    def apply(avenue: String, street: String): Intersection = new Intersection(avenue.toUpperCase, street)

    /** With this you can replace `Intersection("A", "1")` with `i"A1"` */
    implicit class IntersectionInterpolator(sc: StringContext) {
      //noinspection ScalaUnusedSymbol, without arguments there is a warn when using it
      def i(subs: Any*): Intersection = {
        val Intersection.parser(avenue, street) = sc.parts.head
        Intersection(avenue, street)
      }
    }
  }

  /** One-way segment of the road bewteen two intersections with the time it takes to traverse it */
  case class RoadSegment(transitTime: Double, start: Intersection, end: Intersection) {
    lazy val withoutTime: (Intersection, Intersection) = (start, end)

    override def toString: String = s"$toStringWithoutTime in $transitTime"

    def toStringWithoutTime: String = s"$start ~> $end"
  }

  /**
   * Works in conjuction with [[IntersectionPairOperator]] and [[Intersection.IntersectionInterpolator]]
   * to replace `RoadSegment(20.12, Intersection("B", "1"), Intersection("A", "1"))` with `i"B1" ~> i"A1" takes 1`
   */
  implicit class RoadSegmentOperator(start: Intersection) {
    def ~>(end: Intersection): (Intersection, Intersection) = (start, end)
  }

  /**
   * Works in conjuction with [[RoadSegmentOperator]] and [[Intersection.IntersectionInterpolator]]
   * to replace `RoadSegment(20.12, Intersection("B", "1"), Intersection("A", "1"))` with `i"B1" ~> i"A1" takes 1`
   */
  implicit class IntersectionPairOperator(segmentWithoutTime: (Intersection, Intersection)) {
    def in(transitTime: Double): RoadSegment = RoadSegment(start = segmentWithoutTime._1, end = segmentWithoutTime._2, transitTime = transitTime)
  }

  val t: (Intersection, Intersection) = Intersection("A", "1") -> Intersection("B", "1")
  val r: RoadSegment = t in 123

  Map("B1" -> Intersection("B", "1"))
}
