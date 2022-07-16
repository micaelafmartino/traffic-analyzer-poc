package co.topl.traffic.parser

import co.topl.traffic.Fixture
import co.topl.traffic.model.cityNetwork.Intersection.IntersectionInterpolator
import co.topl.traffic.model.json.TrafficMeasurement
import org.scalatest.TryValues._
import org.scalatest.featurespec.AsyncFeatureSpec
import org.scalatest.matchers.should.Matchers
import play.api.libs.json.Json

import java.io.PrintWriter
import java.nio.file.Files
import scala.io.Source
import scala.util.Using

class TrafficMeasurementsParserSpec extends AsyncFeatureSpec with Matchers with Fixture {
  Feature("avgTransitTime") {
    Scenario("avgTransitTime") {
      val measurements = List(
        TrafficMeasurement(startAvenue = "A", startStreet = "2", transitTime = 56.234, endAvenue = "A", endStreet = "2"),
        TrafficMeasurement(startAvenue = "A", startStreet = "2", transitTime = 48.066, endAvenue = "A", endStreet = "2"),
        TrafficMeasurement(startAvenue = "A", startStreet = "2", transitTime = 34.3, endAvenue = "A", endStreet = "2"),
      )
      TrafficMeasurementsParser("").avgTransitTime(measurements) shouldBe 46.2 +- 0.000000000001
    }
  }

  Feature("readMeasurementsJson") {
    Scenario("fails on invalid url/file") {
      val tempFileUrl = tempFile("""{ "someKey" : "wrong json type""}""")

      TrafficMeasurementsParser("").readMeasurementsJson().isFailure shouldBe true
      TrafficMeasurementsParser("file:non-existent-file.abc").readMeasurementsJson().isFailure shouldBe true
      TrafficMeasurementsParser(tempFileUrl).readMeasurementsJson().isFailure shouldBe true
    }

    Scenario("success on valid url and file") {
      TrafficMeasurementsParser(tempFile(sampleJson)).readMeasurementsJson().success.value shouldBe Json.toJson(parsedSample)
    }
  }

  Feature("readMeasurements") {
    Scenario("fails on invalid url/file") {
      val tempFileUrl = tempFile("""{ "someKey" : "wrong json type""}""")

      TrafficMeasurementsParser("").readMeasurements().isFailure shouldBe true
      TrafficMeasurementsParser("file:non-existent-file.abc").readMeasurements().isFailure shouldBe true
      TrafficMeasurementsParser(tempFileUrl).readMeasurements().isFailure shouldBe true
    }

    Scenario("success on valid url and file") {
      TrafficMeasurementsParser(tempFile(sampleJson)).readMeasurements().success.value shouldBe parsedSample
    }
  }

  Feature("roadSegments") {
    Scenario("merges same segment from different measurements with the average transit time") {
      val segments = TrafficMeasurementsParser(tempFile(sampleJson)).roadSegments().success.value

      // round for easy comparison
      segments.map(s => s.copy(transitTime = s.transitTime.round)) shouldBe Set(i"A1" ~> i"B1" in 28, i"A2" ~> i"A1" in 58, i"A2" ~> i"B2" in 48)
    }
  }

  private def tempFile(content: String): String = {
    val tempFileUrl = Files.createTempFile(null, null).toUri.toString
    Using(new PrintWriter(tempFileUrl.replaceFirst("file:", "")))(_.write(content))

    Using(Source.fromURL(tempFileUrl))(_.mkString).success.value shouldBe content // check the file exists and has the content in it
    tempFileUrl
  }
}