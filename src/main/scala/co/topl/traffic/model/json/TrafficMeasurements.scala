package co.topl.traffic.model.json

import co.topl.traffic.model.cityNetwork.{Intersection, RoadSegment}
import play.api.libs.json._

case class TrafficMeasurements(trafficMeasurements: List[TimedMeasurements])

case class TimedMeasurements(measurementTime: Long, measurements: List[TrafficMeasurement])

case class TrafficMeasurement(startAvenue: String, startStreet: String, transitTime: Double, endAvenue: String, endStreet: String) {
  lazy val segment = RoadSegment(
    transitTime = transitTime,
    start = Intersection(avenue = startAvenue, street = startStreet),
    end = Intersection(avenue = endAvenue, street = endStreet)
  )
}

object TrafficMeasurements {
  implicit lazy val trafficMeasurementsJsonReader: Reads[TrafficMeasurement] = Json.reads
  implicit lazy val timedMeasurementsJsonReader: Reads[TimedMeasurements] = Json.reads
  implicit lazy val trafficMeasurementJsonReader: Reads[TrafficMeasurements] = Json.reads
}
