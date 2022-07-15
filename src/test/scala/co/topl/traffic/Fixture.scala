package co.topl.traffic

import co.topl.traffic.model.json.{TimedMeasurements, TrafficMeasurement, TrafficMeasurements}
import play.api.libs.json.{Json, Writes}

trait Fixture {
  val sampleJson =
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

  val parsedSample: TrafficMeasurements = TrafficMeasurements(List(
    TimedMeasurements(86544, List(
      TrafficMeasurement(startAvenue = "A", startStreet = "1", transitTime = 28.000987663134676, endAvenue = "B", endStreet = "1"),
      TrafficMeasurement(startAvenue = "A", startStreet = "2", transitTime = 59.71131185379898, endAvenue = "A", endStreet = "1"),
    )),
    TimedMeasurements(86575, List(
      TrafficMeasurement(startAvenue = "A", startStreet = "2", transitTime = 56.23494269309719, endAvenue = "A", endStreet = "1"),
      TrafficMeasurement(startAvenue = "A", startStreet = "2", transitTime = 48.084859707872376, endAvenue = "B", endStreet = "2"),
    )),
  ))

  protected implicit lazy val timedMeasurementsWriter: Writes[TimedMeasurements] = Json.writes
  protected implicit lazy val trafficMeasurementWriter: Writes[TrafficMeasurement] = Json.writes
  protected implicit lazy val trafficMeasurementsWriter: Writes[TrafficMeasurements] = Json.writes
}
