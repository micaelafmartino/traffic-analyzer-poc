package co.topl.traffic.model.json

import play.api.libs.json.{Json, Writes}

case class BestPath(source: String, target: String, totalTransitTime: Double, path: List[String])

object BestPath {
  implicit val bestPathJsonReader: Writes[BestPath] = Json.writes
}
