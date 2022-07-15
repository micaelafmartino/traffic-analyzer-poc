package co.topl.traffic.model.json

import co.topl.traffic.Fixture
import co.topl.traffic.model.cityNetwork.{Intersection, RoadSegment}
import org.scalatest.featurespec.AsyncFeatureSpec
import org.scalatest.matchers.should.Matchers
import play.api.libs.json._

class TrafficMeasurementsSpec extends AsyncFeatureSpec with Matchers with Fixture {
  Feature("readability") {
    Scenario("TrafficMeasurements is readable") {
      Json.parse(sampleJson).as[TrafficMeasurements] shouldBe parsedSample
    }
  }

  Feature("model conversions") {
    Scenario("TrafficMeasurement to RoadSegment") {
      TrafficMeasurement(startAvenue = "A", startStreet = "1", transitTime = 28.000987663134676, endAvenue = "B", endStreet = "1").segment shouldBe
        RoadSegment(28.000987663134676, Intersection(avenue = "A", street = "1"), Intersection(avenue = "B", street = "1"))
    }
  }
}