package co.topl.traffic.model.json

import org.scalatest.featurespec.AsyncFeatureSpec
import org.scalatest.matchers.should.Matchers
import play.api.libs.json._

class BestRouteSpec extends AsyncFeatureSpec with Matchers {
  Feature("writability") {
    Scenario("BestRoute is writable") {
      Json.toJson(BestRoute("A1", "B2", 100.12, List("A1 -> B2"))).toString() shouldBe """{"source":"A1","target":"B2","totalTransitTime":100.12,"path":["A1 -> B2"]}""".stripMargin
    }
  }
}