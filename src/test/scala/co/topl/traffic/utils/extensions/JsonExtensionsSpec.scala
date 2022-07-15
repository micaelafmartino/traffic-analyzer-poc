package co.topl.traffic.utils.extensions

import org.scalatest.featurespec.AsyncFeatureSpec
import org.scalatest.matchers.should.Matchers
import play.api.libs.json._

import scala.util.Success

class JsonExtensionsSpec extends AsyncFeatureSpec with Matchers {
  Feature("asTry") {
    Scenario("valid json and type") {
      JsObject(Seq("a" -> JsNumber(1), "b" -> JsNumber(2))).asTry[Test] shouldBe Success(Test(1, 2))
      JsObject.empty.asTry[TestOption] shouldBe Success(TestOption(None, None))
    }
    Scenario("invalid") {
      assert(JsObject(Seq("a" -> JsNumber(1), "b" -> JsNumber(2))).asTry[Test2].isFailure)
      assert(JsObject(Seq("a" -> JsNumber(1), "b" -> JsString("2"))).asTry[Test].isFailure)
    }
  }

  case class Test(a: Int, b: Int)

  case class Test2(a: String, b: Int)

  case class TestOption(a: Option[Int], b: Option[Int])

  private implicit val testFormat: Format[Test] = Json.format[Test]
  private implicit val test2Format: Format[Test2] = Json.format[Test2]
  private implicit val testOptionFormat: Format[TestOption] = Json.format[TestOption]
}