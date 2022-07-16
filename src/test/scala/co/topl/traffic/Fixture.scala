package co.topl.traffic

import co.topl.traffic.analyzer.TrafficAnalyzer
import co.topl.traffic.model.cityNetwork.{Intersection, RoadSegment}
import co.topl.traffic.model.json.{TimedMeasurements, TrafficMeasurement, TrafficMeasurements}
import play.api.libs.json.{Json, Writes}

trait Fixture {
  lazy val sampleJson: String =
    """
      |{
      |  "trafficMeasurements" : [
      |    {
      |      "measurementTime" : 86544,
      |      "measurements" : [
      |        {
      |          "startAvenue" : "A",
      |          "startStreet" : "1",
      |          "transitTime" : 28.000987663134676,
      |          "endAvenue" : "B",
      |          "endStreet" : "1"
      |        }, {
      |          "startAvenue" : "A",
      |          "startStreet" : "2",
      |          "transitTime" : 59.71131185379898,
      |          "endAvenue" : "A",
      |          "endStreet" : "1"
      |        }
      |      ]
      |    }, {
      |      "measurementTime" : 86575,
      |      "measurements" : [
      |        {
      |          "startAvenue" : "A",
      |          "startStreet" : "2",
      |          "transitTime" : 56.23494269309719,
      |          "endAvenue" : "A",
      |          "endStreet" : "1"
      |        }, {
      |          "startAvenue" : "A",
      |          "startStreet" : "2",
      |          "transitTime" : 48.084859707872376,
      |          "endAvenue" : "B",
      |          "endStreet" : "2"
      |        }
      |      ]
      |    }
      |  ]
      |}
      |""".stripMargin

  lazy val parsedSample: TrafficMeasurements = TrafficMeasurements(List(
    TimedMeasurements(86544, List(
      TrafficMeasurement(startAvenue = "A", startStreet = "1", transitTime = 28.000987663134676, endAvenue = "B", endStreet = "1"),
      TrafficMeasurement(startAvenue = "A", startStreet = "2", transitTime = 59.71131185379898, endAvenue = "A", endStreet = "1"),
    )),
    TimedMeasurements(86575, List(
      TrafficMeasurement(startAvenue = "A", startStreet = "2", transitTime = 56.23494269309719, endAvenue = "A", endStreet = "1"),
      TrafficMeasurement(startAvenue = "A", startStreet = "2", transitTime = 48.084859707872376, endAvenue = "B", endStreet = "2"),
    )),
  ))

  object intersections {
    val A1 = Intersection("A", "1")
    val A2 = Intersection("A", "2")
    val A3 = Intersection("A", "3")
    val B1 = Intersection("B", "1")
    val B2 = Intersection("B", "2")
    val B3 = Intersection("B", "3")
    val allintersections = Set(A1, A2, A3, B1, B2, B3)
  }
  import intersections._

  lazy val target: Intersection = B2

  lazy val segments: Set[RoadSegment] = Set(
    RoadSegment(1, B1, A1),
    RoadSegment(2, A1, A2),
    RoadSegment(3, A2, A3),
    RoadSegment(5, A3, B3),
    RoadSegment(6.1, B3, B2),
    RoadSegment(4, A2, B2),
    RoadSegment(7, B2, B1),
  )

  lazy val leadingSegments: Map[Intersection, List[RoadSegment]] = Map(
    A1 -> List(RoadSegment(1, B1, A1)),
    A2 -> List(RoadSegment(2, A1, A2)),
    A3 -> List(RoadSegment(3, A2, A3)),
    B1 -> List(RoadSegment(7, B2, B1)),
    B2 -> List(RoadSegment(4, A2, B2), RoadSegment(6.1, B3, B2)),
    B3 -> List(RoadSegment(5, A3, B3)),
  )

  lazy val analyzer: TrafficAnalyzer = TrafficAnalyzer.from(segments, target).get

  protected implicit lazy val timedMeasurementsWriter: Writes[TimedMeasurements] = Json.writes
  protected implicit lazy val trafficMeasurementWriter: Writes[TrafficMeasurement] = Json.writes
  protected implicit lazy val trafficMeasurementsWriter: Writes[TrafficMeasurements] = Json.writes
}
