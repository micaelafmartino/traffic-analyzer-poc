package co.topl.traffic.parser

import co.topl.traffic.model.cityNetwork.RoadSegment
import co.topl.traffic.model.json.{TrafficMeasurement, TrafficMeasurements}
import co.topl.traffic.utils.extensions.JsValueExtensions
import play.api.libs.json._

import scala.io.Source
import scala.util.{Try, Using}

/** Util class to read and parse a traffic measurements file */
case class TrafficMeasurementsParser(url: String) {
  /** Loads and parses the measurements file pointed by the given URL */
  def readMeasurementsJson(): Try[JsValue] = Using(Source.fromURL(url))(_.mkString).map(Json.parse)

  /** Same as [[readMeasurementsJson]] but instead of raw [[JsValue]] returns a [[TrafficMeasurements]] object */
  def readMeasurements(): Try[TrafficMeasurements] = readMeasurementsJson().flatMap(_.asTry[TrafficMeasurements])

  /** Takes all measurements from [[readMeasurements]] and returns each [[RoadSegment]] with the average transit time between all measurements */
  def roadSegments(): Try[Set[RoadSegment]] = readMeasurements().map(_
    .trafficMeasurements
    .flatMap(_.measurements)
    .groupBy(_.segment.unmeasured)
    .map { case (segment, measurements) => RoadSegment(avgTransitTime(measurements), segment.start, segment.end) }
    .toSet
  )

  private[parser] def avgTransitTime(measurements: List[TrafficMeasurement]): Double = measurements.map(_.transitTime).sum / measurements.size.toDouble
}