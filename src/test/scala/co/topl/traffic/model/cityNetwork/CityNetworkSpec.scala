package co.topl.traffic.model.cityNetwork

import org.scalatest.featurespec.AsyncFeatureSpec
import org.scalatest.matchers.should.Matchers

class CityNetworkSpec extends AsyncFeatureSpec with Matchers {
  Feature("construction") {
    Scenario("Intersection's avenues are always uppercase") {
      Intersection(avenue = "A", street = "1").avenue shouldBe "A"
      Intersection(avenue = "a", street = "1").avenue shouldBe "A"
    }
  }

  Feature("toString") {
    Scenario("Intesection") {
      Intersection(avenue = "A", street = "1").toString shouldBe "A1"
      Intersection(avenue = "d", street = "22").toString shouldBe "D22"
    }

    Scenario("RoadSegment") {
      RoadSegment(0, Intersection("A", "1"), Intersection("A", "2")).toString shouldBe "A1 -> A2"
      RoadSegment(0, Intersection("b", "23"), Intersection("k", "15")).toString shouldBe "B23 -> K15"
    }
  }

  Feature("withoutTime") {
    Scenario("returns a tuple with the starting intersection and the ending one") {
      RoadSegment(0, Intersection("A", "1"), Intersection("A", "2")).withoutTime shouldBe(Intersection("A", "1"), Intersection("A", "2"))
      RoadSegment(0, Intersection("b", "23"), Intersection("k", "15")).withoutTime shouldBe(Intersection("b", "23"), Intersection("k", "15"))
    }
  }
}