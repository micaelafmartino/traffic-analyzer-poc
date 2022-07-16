package co.topl.traffic.analyzer

import co.topl.traffic.Fixture
import co.topl.traffic.analyzer.TrafficAnalyzer.PathToTarget
import co.topl.traffic.errors.TrafficAnalyzerInitError
import co.topl.traffic.model.cityNetwork.{Intersection, RoadSegment}
import co.topl.traffic.parser.TrafficMeasurementsParser
import org.scalatest.TryValues._
import org.scalatest.featurespec.AsyncFeatureSpec
import org.scalatest.matchers.should.Matchers

class TrafficAnalyzerSpec extends AsyncFeatureSpec with Matchers with Fixture {

  import intersections._

  Feature("construction") {
    val somePathToTarget = List(RoadSegment(10, A2, target))
    val pathToTarget = Map(target -> PathToTarget.init(target), A2 -> PathToTarget(List(RoadSegment(10, A2, target), RoadSegment(0, target, target))))
    val minLeadingSegments = Map(target -> somePathToTarget)

    Scenario("PathToTarget with empty segments") {
      a[TrafficAnalyzerInitError] should be thrownBy PathToTarget(List.empty)
    }

    Scenario("PathToTarget.init must set an already shortest self path for the target") {
      PathToTarget.init(A1) shouldBe PathToTarget(List(RoadSegment(0, A1, A1)), alreadyShortest = true)
    }

    Scenario("TrafficAnalyzer cannot be created if the target doesn't have at least one path leading to it") {
      TrafficAnalyzer(minLeadingSegments, target, target, pathToTarget).isSuccess shouldBe true
      TrafficAnalyzer(Map.empty, target, target, pathToTarget).failure.exception shouldBe a[TrafficAnalyzerInitError]
      TrafficAnalyzer(Map(target -> List.empty), target, target, pathToTarget).failure.exception shouldBe a[TrafficAnalyzerInitError]
      TrafficAnalyzer(Map(A2 -> somePathToTarget), target, target, pathToTarget).failure.exception shouldBe a[TrafficAnalyzerInitError]
    }

    Scenario("TrafficAnalyzer cannot be created if the current intersection doesn't have a calculated path to target") {
      TrafficAnalyzer(minLeadingSegments, target, A2, pathToTarget).isSuccess shouldBe true
      TrafficAnalyzer(minLeadingSegments, target, A2, Map.empty).failure.exception shouldBe a[TrafficAnalyzerInitError]
      TrafficAnalyzer(minLeadingSegments, target, A2, Map(target -> PathToTarget.init(target))).failure.exception shouldBe a[TrafficAnalyzerInitError]
    }

    Scenario("TrafficAnalyzer cannot be created if the self path to target is not flag as shortest") {
      TrafficAnalyzer(minLeadingSegments, target, A2, pathToTarget).isSuccess shouldBe true
      //noinspection RedundantDefaultArgument
      val notShortestSelfPath = PathToTarget(List(RoadSegment(0, target, target)), alreadyShortest = false)
      TrafficAnalyzer(minLeadingSegments, target, A2, Map(target -> notShortestSelfPath)).failure.exception shouldBe a[TrafficAnalyzerInitError]
    }

    Scenario("TrafficAnalyzer infers the leadingSegments to each intersection from a set of segments") {
      TrafficAnalyzer.from(segments, target).success.value.leadingSegments shouldBe leadingSegments
    }
  }

  Feature("PathToTarget#>>:") {
    val origPath = PathToTarget.init(A1)

    Scenario("prepend a valid segment to an existent PathToTarget") {
      val newSegment = RoadSegment(12.123, A2, A1)
      val newPath = newSegment >>: origPath

      newPath.start shouldBe newSegment.start
      newPath.segments shouldBe List(
        RoadSegment(12.123, A2, A1),
        RoadSegment(0, A1, A1),
      )
      newPath.alreadyShortest shouldBe false
    }

    Scenario("prepend an invalid segment to an existent PathToTarget does nothing") {
      val newSegment = RoadSegment(12.123, A2, A3)
      (newSegment >>: origPath) shouldBe origPath
    }
  }

  Feature("PathToTarget#totalTransitTime") {
    Scenario("sum of all segment's transit times") {
      PathToTarget(List(
        RoadSegment(12.123, B2, B1),
        RoadSegment(123, B1, A1),
        RoadSegment(0, A1, A1),
      )).totalTransitTime shouldBe 135.123
    }
  }

  Feature("PathToTarget#shortest/shorterThan") {
    Scenario("path with less totalTransitTime is shorter") {
      val theShortest = PathToTarget(List(
        RoadSegment(3, B2, B1),
        RoadSegment(13, B1, A1),
        RoadSegment(0, A1, A1),
      ))

      val theLongest = PathToTarget(List(
        RoadSegment(12.123, B2, B1),
        RoadSegment(123, B1, A1),
        RoadSegment(0, A1, A1),
      ))

      theShortest.totalTransitTime shouldBe 16
      theLongest.totalTransitTime shouldBe 135.123

      theShortest.shorterThan(theLongest) shouldBe true
      theLongest.shorterThan(theShortest) shouldBe false

      theShortest.shortest(theLongest) shouldBe theShortest
    }
  }

  Feature("TrafficAnalyzer#complete") {
    Scenario("TrafficAnalyzer is complete when all intersections have a PathToTarget") {
      analyzer.complete shouldBe false
      analyzer.copy(pathToTarget = allintersections.map(_ -> PathToTarget.init(target)).toMap).complete shouldBe true
    }
  }

  Feature("TrafficAnalyzer#selectNewCurrent") {
    Scenario("current intersection's path is marked as shortest and the new intersection is set as current") {
      analyzer.current shouldBe target
      analyzer.pathToTarget.contains(A2) shouldBe false
      analyzer.pathToTarget.contains(B3) shouldBe false

      val udpdatedAnalyzer = analyzer.selectNewCurrent(A2)

      udpdatedAnalyzer.current shouldBe A2
      udpdatedAnalyzer.pathToTarget(A2).alreadyShortest shouldBe true
      udpdatedAnalyzer.pathToTarget(B3).alreadyShortest shouldBe false
    }
  }

  Feature("TrafficAnalyzer#nextCurrent") {
    Scenario("fails if pathToTarget is empty") {
      a[NoSuchElementException] should be thrownBy analyzer.copy(pathToTarget = Map.empty).nextCurrent
    }

    Scenario("None if pathToTarget only have shortest paths") {
      val someSegment = RoadSegment(0, target, target)
      analyzer.copy(pathToTarget = allintersections.map(_ -> PathToTarget(List(someSegment), alreadyShortest = true)).toMap).nextCurrent shouldBe None
    }

    Scenario("return the shortest of all non shortest paths") {
      val pathToTarget = Map(
        target -> PathToTarget(List(RoadSegment(0, target, target)), alreadyShortest = true),
        A2 -> PathToTarget(List(RoadSegment(4, A2, target), RoadSegment(0, target, target)), alreadyShortest = true),
        B3 -> PathToTarget(List(RoadSegment(6.1, B3, target), RoadSegment(0, target, target))), // 6.1
        A1 -> PathToTarget(List(RoadSegment(2, A1, A2), RoadSegment(4, A2, target), RoadSegment(0, target, target))), // 6, shortest than B3
      )
      analyzer.copy(leadingSegments = leadingSegments, current = A2, pathToTarget = pathToTarget).nextCurrent shouldBe Some(A1)
    }
  }

  Feature("TrafficAnalyzer#updatedPathsViaCurrent") {
    Scenario("it should add paths to intersections leading to current on each iteration") {
      // pre conditions
      leadingSegments(A2) shouldBe List(RoadSegment(2, A1, A2))
      leadingSegments(target) shouldBe List(RoadSegment(4, A2, target), RoadSegment(6.1, B3, target))

      // initial state
      analyzer.pathToTarget shouldBe Map(target -> PathToTarget.init(target))

      // first iteration with current = target
      analyzer.nextCurrent shouldBe Some(A2) // because A2 -> target [4] vs B3 -> target [6]
      analyzer.updatedPathsViaCurrent shouldBe Map(
        target -> PathToTarget(List(RoadSegment(0, target, target)), alreadyShortest = true),
        A2 -> PathToTarget(List(RoadSegment(4, A2, target), RoadSegment(0, target, target))),
        B3 -> PathToTarget(List(RoadSegment(6.1, B3, target), RoadSegment(0, target, target))),
      )

      // second iteration with current = A2
      val secondIteration = analyzer.selectNewCurrent(A2)
      secondIteration.nextCurrent shouldBe Some(A1) // because A1 -> A2 -> target [6] vs B3 -> target [6.1]
      secondIteration.updatedPathsViaCurrent shouldBe Map(
        target -> PathToTarget(List(RoadSegment(0, target, target)), alreadyShortest = true),
        A2 -> PathToTarget(List(RoadSegment(4, A2, target), RoadSegment(0, target, target)), alreadyShortest = true),
        B3 -> PathToTarget(List(RoadSegment(6.1, B3, target), RoadSegment(0, target, target))),
        A1 -> PathToTarget(List(RoadSegment(2, A1, A2), RoadSegment(4, A2, target), RoadSegment(0, target, target))),
      )

      // third iteration with current = A1
      val thirdIteration = secondIteration.selectNewCurrent(A1)
      thirdIteration.nextCurrent shouldBe Some(B3)
      thirdIteration.updatedPathsViaCurrent should contain allElementsOf Map(
        target -> PathToTarget(List(RoadSegment(0, target, target)), alreadyShortest = true),
        A2 -> PathToTarget(List(RoadSegment(4, A2, target), RoadSegment(0, target, target)), alreadyShortest = true),
        B3 -> PathToTarget(List(RoadSegment(6.1, B3, target), RoadSegment(0, target, target))),
        A1 -> PathToTarget(List(RoadSegment(2, A1, A2), RoadSegment(4, A2, target), RoadSegment(0, target, target)), alreadyShortest = true),
      )
    }
  }

  Feature("TrafficAnalyzer#shortestPathFrom") {
    Scenario("it should find the shortest path from source to target and stops when it finds it") {
      val r = analyzer._shortestPathFrom(B1)
      r.current shouldBe B1
      r.nextCurrent shouldNot be(None)
      analyzer.shortestPathFrom(B1) shouldBe Some(7, List(RoadSegment(1, B1, A1), RoadSegment(2, A1, A2), RoadSegment(4, A2, B2), RoadSegment(0, B2, B2)))
    }

    Scenario("it should return None if the target is unreacheable from the source") {
      val inexistentIntersection = Intersection("Z", "99")
      analyzer._shortestPathFrom(inexistentIntersection).nextCurrent shouldBe None
      analyzer.shortestPathFrom(inexistentIntersection) shouldBe None
    }
  }

  Feature("TrafficAnalyzer#shortestPaths") {
    Scenario("it should find the shortest path to target for each intersection") {
      val r = analyzer.shortestPaths()
      leadingSegments.keys.foreach(r.pathToTarget.contains)
      r.nextCurrent shouldBe None
    }
  }
}