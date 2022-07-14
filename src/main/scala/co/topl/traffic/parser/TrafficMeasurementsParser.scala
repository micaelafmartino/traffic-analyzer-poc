package co.topl.traffic.parser

import co.topl.traffic.model.json.TrafficMeasurements
import co.topl.traffic.utils.extensions.JsValueExtensions
import play.api.libs.json._

import scala.io.Source
import scala.util.{Try, Using}

case class TrafficMeasurementsParser(url: String) {
  def readMeasurementsJson(): Try[JsValue] = Using(Source.fromURL(url))(_.mkString).map(Json.parse)

  def readMeasurements(): Try[TrafficMeasurements] = readMeasurementsJson().flatMap(_.asTry[TrafficMeasurements])
}