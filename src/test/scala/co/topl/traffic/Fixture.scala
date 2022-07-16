package co.topl.traffic

import co.topl.traffic.analyzer.TrafficAnalyzer
import co.topl.traffic.model.cityNetwork.Intersection.IntersectionInterpolator
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

  val intersections = Set(i"A1", i"A2", i"A3", i"B1", i"B2", i"B3")

  val target: Intersection = i"B2"

  val segments: Set[RoadSegment] = Set(
    i"B1" ~> i"A1" in 1,
    i"A1" ~> i"A2" in 2,
    i"A2" ~> i"A3" in 3,
    i"A3" ~> i"B3" in 5,
    i"B3" ~> i"B2" in 6.1,
    i"A2" ~> i"B2" in 4,
    i"B2" ~> i"B1" in 7,
  )

  lazy val leadingSegments: Map[Intersection, List[RoadSegment]] = Map(
    i"A1" -> List(i"B1" ~> i"A1" in 1),
    i"A2" -> List(i"A1" ~> i"A2" in 2),
    i"A3" -> List(i"A2" ~> i"A3" in 3),
    i"B1" -> List(i"B2" ~> i"B1" in 7),
    i"B2" -> List(i"A2" ~> i"B2" in 4, i"B3" ~> i"B2" in 6.1),
    i"B3" -> List(i"A3" ~> i"B3" in 5),
  )

  lazy val analyzer: TrafficAnalyzer = TrafficAnalyzer.from(segments, target).get

  protected implicit lazy val timedMeasurementsWriter: Writes[TimedMeasurements] = Json.writes
  protected implicit lazy val trafficMeasurementWriter: Writes[TrafficMeasurement] = Json.writes
  protected implicit lazy val trafficMeasurementsWriter: Writes[TrafficMeasurements] = Json.writes
}
