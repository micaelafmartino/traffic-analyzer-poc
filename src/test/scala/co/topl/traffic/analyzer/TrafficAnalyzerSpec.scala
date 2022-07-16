package co.topl.traffic.analyzer

import co.topl.traffic.Fixture
import co.topl.traffic.analyzer.TrafficAnalyzer.PathToTarget
import co.topl.traffic.errors.TrafficAnalyzerInitError
import co.topl.traffic.model.cityNetwork.{Intersection, RoadSegment}
import org.scalatest.TryValues._
import org.scalatest.featurespec.AsyncFeatureSpec
import org.scalatest.matchers.should.Matchers

class TrafficAnalyzerSpec extends AsyncFeatureSpec with Matchers with Fixture {
  Feature("construction") {
    val target = Intersection("A", "1")
    val otherIntersection = Intersection("A", "2")
    val somePathToTarget = List(RoadSegment(10, otherIntersection, target))
    val pathToTarget = Map(target -> PathToTarget.init(target), otherIntersection -> PathToTarget(List(RoadSegment(10, otherIntersection, target), RoadSegment(0, target, target))))
    val leadingSegments = Map(target -> somePathToTarget)

    Scenario("PathToTarget with empty segments") {
      a[TrafficAnalyzerInitError] should be thrownBy PathToTarget(List.empty)
    }

    Scenario("PathToTarget.init must set an already shortest self path for the target") {
      PathToTarget.init(Intersection("A", "1")) shouldBe PathToTarget(List(RoadSegment(0, Intersection("A", "1"), Intersection("A", "1"))), alreadyShortest = true)
    }

    Scenario("TrafficAnalyzer cannot be created if the target doesn't have at least one path leading to it") {
      TrafficAnalyzer(leadingSegments, target, target, pathToTarget).isSuccess shouldBe true
      TrafficAnalyzer(Map.empty, target, target, pathToTarget).failure.exception shouldBe a[TrafficAnalyzerInitError]
      TrafficAnalyzer(Map(target -> List.empty), target, target, pathToTarget).failure.exception shouldBe a[TrafficAnalyzerInitError]
      TrafficAnalyzer(Map(otherIntersection -> somePathToTarget), target, target, pathToTarget).failure.exception shouldBe a[TrafficAnalyzerInitError]
    }

    Scenario("TrafficAnalyzer cannot be created if the current intersection doesn't have a calculated path to target") {
      TrafficAnalyzer(leadingSegments, target, otherIntersection, pathToTarget).isSuccess shouldBe true
      TrafficAnalyzer(leadingSegments, target, otherIntersection, Map.empty).failure.exception shouldBe a[TrafficAnalyzerInitError]
      TrafficAnalyzer(leadingSegments, target, otherIntersection, Map(target -> PathToTarget.init(target))).failure.exception shouldBe a[TrafficAnalyzerInitError]
    }

    Scenario("TrafficAnalyzer cannot be created if the self path to target is not flag as shortest") {
      TrafficAnalyzer(leadingSegments, target, otherIntersection, pathToTarget).isSuccess shouldBe true
      //noinspection RedundantDefaultArgument
      val notShortestSelfPath = PathToTarget(List(RoadSegment(0, target, target)), alreadyShortest = false)
      TrafficAnalyzer(leadingSegments, target, otherIntersection, Map(target -> notShortestSelfPath)).failure.exception shouldBe a[TrafficAnalyzerInitError]
    }

    Scenario("TrafficAnalyzer infers the leadingSegments to each intersection from a set of segments") {
      val segments = Set(
        RoadSegment(10, Intersection("B", "1"), Intersection("A", "1")),
        RoadSegment(10, Intersection("A", "1"), Intersection("A", "2")),
        RoadSegment(10, Intersection("A", "2"), Intersection("A", "3")),
        RoadSegment(10, Intersection("A", "2"), Intersection("B", "2")),
        RoadSegment(10, Intersection("A", "3"), Intersection("B", "3")),
        RoadSegment(10, Intersection("B", "3"), Intersection("B", "2")),
        RoadSegment(10, Intersection("B", "2"), Intersection("B", "1")),
      )
      TrafficAnalyzer.from(segments, target).success.value.leadingSegments shouldBe Map(
        Intersection("A", "1") -> List(RoadSegment(10, Intersection("B", "1"), Intersection("A", "1"))),
        Intersection("A", "2") -> List(RoadSegment(10, Intersection("A", "1"), Intersection("A", "2"))),
        Intersection("A", "3") -> List(RoadSegment(10, Intersection("A", "2"), Intersection("A", "3"))),
        Intersection("B", "1") -> List(RoadSegment(10, Intersection("B", "2"), Intersection("B", "1"))),
        Intersection("B", "2") -> List(RoadSegment(10, Intersection("B", "3"), Intersection("B", "2")), RoadSegment(10, Intersection("A", "2"), Intersection("B", "2"))),
        Intersection("B", "3") -> List(RoadSegment(10, Intersection("A", "3"), Intersection("B", "3"))),
      )
    }
  }

  Feature("PathToTarget#>>:") {
    val origPath = PathToTarget.init(Intersection("A", "1"))

    Scenario("prepend a valid segment to an existent PathToTarget") {
      val newSegment = RoadSegment(12.123, Intersection("A", "2"), Intersection("A", "1"))
      val newPath = newSegment >>: origPath

      newPath.start shouldBe newSegment.start
      newPath.segments shouldBe List(
        RoadSegment(12.123, Intersection("A", "2"), Intersection("A", "1")),
        RoadSegment(0, Intersection("A", "1"), Intersection("A", "1")),
      )
      newPath.alreadyShortest shouldBe false
    }

    Scenario("prepend an invalid segment to an existent PathToTarget does nothing") {
      val newSegment = RoadSegment(12.123, Intersection("A", "2"), Intersection("A", "3"))
      (newSegment >>: origPath) shouldBe origPath
    }
  }

  Feature("PathToTarget#totalTransitTime") {
    Scenario("sum of all segment's transit times") {
      PathToTarget(List(
        RoadSegment(12.123, Intersection("B", "2"), Intersection("B", "1")),
        RoadSegment(123, Intersection("B", "1"), Intersection("A", "1")),
        RoadSegment(0, Intersection("A", "1"), Intersection("A", "1")),
      )).totalTransitTime shouldBe 135.123
    }
  }

  Feature("PathToTarget#shortest/shorterThan") {
    Scenario("path with less totalTransitTime is shorter") {
      val theShortest = PathToTarget(List(
        RoadSegment(3, Intersection("B", "2"), Intersection("B", "1")),
        RoadSegment(13, Intersection("B", "1"), Intersection("A", "1")),
        RoadSegment(0, Intersection("A", "1"), Intersection("A", "1")),
      ))

      val theLongest = PathToTarget(List(
        RoadSegment(12.123, Intersection("B", "2"), Intersection("B", "1")),
        RoadSegment(123, Intersection("B", "1"), Intersection("A", "1")),
        RoadSegment(0, Intersection("A", "1"), Intersection("A", "1")),
      ))

      theShortest.totalTransitTime shouldBe 16
      theLongest.totalTransitTime shouldBe 135.123

      theShortest.shorterThan(theLongest) shouldBe true
      theLongest.shorterThan(theShortest) shouldBe false

      theShortest.shortest(theLongest) shouldBe theShortest
    }
  }
}