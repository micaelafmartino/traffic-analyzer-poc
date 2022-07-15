package co.topl.traffic.model.json

import play.api.libs.json.{Json, Writes}

case class BestRoute(source: String, target: String, totalTransitTime: Double, path: List[String])

object BestRoute {
  implicit val bestPathJsonReader: Writes[BestRoute] = Json.writes
}
