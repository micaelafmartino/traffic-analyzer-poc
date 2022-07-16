package co.topl.traffic.model.cityNetwork

import co.topl.traffic.model.cityNetwork.Intersection.IntersectionInterpolator
import org.scalatest.featurespec.AsyncFeatureSpec
import org.scalatest.matchers.should.Matchers

class CityNetworkSpec extends AsyncFeatureSpec with Matchers {
  Feature("construction") {
    Scenario("Intersection's avenues are always uppercase") {
      i"A1".avenue shouldBe "A"
      i"a1".avenue shouldBe "A"
    }
  }

  Feature("toString") {
    Scenario("Intesection") {
      i"A1".toString shouldBe "A1"
      i"d22".toString shouldBe "D22"
    }

    Scenario("RoadSegment") {
      (i"A1" ~> i"A2" in 0.123).toString shouldBe "A1 ~> A2 in 0.123"
      (i"b23" ~> i"k15" in 64.8).toString shouldBe "B23 ~> K15 in 64.8"
    }
  }

  Feature("withoutTime") {
    Scenario("returns a tuple with the starting intersection and the ending one") {
      (i"A1" ~> i"A2" in 0).withoutTime shouldBe(i"A1", i"A2")
      (i"b23" ~> i"k15" in 0).withoutTime shouldBe(i"b23", i"k15")
    }
  }
}