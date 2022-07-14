package co.topl.traffic.model.json

import co.topl.traffic.model.cityNetwork.{Intersection, RoadSegment}
import co.topl.traffic.utils.extensions.JsValueExtensions
import play.api.libs.json._

import scala.io.Source
import scala.util.{Try, Using}

case class TrafficMeasurements(trafficMeasurements: List[TimedMeasurements])

case class TimedMeasurements(measurementTime: Long, measurements: List[TrafficMeasurement])

case class TrafficMeasurement(startAvenue: String, startStreet: String, transitTime: Double, endAvenue: String, endStreet: String) {
  lazy val segment = RoadSegment(
    transitTime = transitTime,
    start = Intersection(street = startStreet, avenue = startAvenue),
    end = Intersection(street = endStreet, avenue = endAvenue)
  )
}

object TrafficMeasurements {
  implicit lazy val trafficMeasurementsJsonReader: Reads[TrafficMeasurement] = Json.reads
  implicit lazy val timedMeasurementsJsonReader: Reads[TimedMeasurements] = Json.reads
  implicit lazy val trafficMeasurementJsonReader: Reads[TrafficMeasurements] = Json.reads

  def readMeasurementsJson(url: String): Try[JsValue] = Using(Source.fromURL(url))(_.mkString).map(Json.parse)

  def readMeasurements(url: String): Try[TrafficMeasurements] = readMeasurementsJson(url).flatMap(_.asTry[TrafficMeasurements])
}