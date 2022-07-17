package co.topl.traffic.model.json

import co.topl.traffic.model.cityNetwork.Intersection.IntersectionInterpolator
import org.scalatest.featurespec.AsyncFeatureSpec
import org.scalatest.matchers.should.Matchers
import play.api.libs.json._

class BestRouteSpec extends AsyncFeatureSpec with Matchers {
  Feature("writability") {
    Scenario("BestRoute is writable") {
      Json.toJson(BestRoute("A1", "B2", 100.12, List((i"A1" ~> i"B2").toString))).toString() shouldBe """{"source":"A1","target":"B2","totalTransitTime":100.12,"path":["A1 ~> B2"]}"""
    }
  }
}