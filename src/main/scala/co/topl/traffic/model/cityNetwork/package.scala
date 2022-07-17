package co.topl.traffic.model

package object cityNetwork {
  /** Intersection between and Avenue and a Street (in a grid pattern) */
  case class Intersection(avenue: String, street: String) {
    def ~>(end: Intersection) = UnmeasuredRoadSegment(this, end)

    override def toString: String = s"$avenue$street"
  }

  object Intersection {
    // At least one letter for the Avenue and at least one digit for the Street
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
    lazy val unmeasured = UnmeasuredRoadSegment(start, end)

    override def toString: String = s"$unmeasured in $transitTime"
  }

  /** Same as a [[RoadSegment]] but without the transit time measurement */
  case class UnmeasuredRoadSegment(start: Intersection, end: Intersection) {
    def in(transitTime: Double): RoadSegment = RoadSegment(start = start, end = end, transitTime = transitTime)

    override def toString: String = s"$start ~> $end"
  }
}
