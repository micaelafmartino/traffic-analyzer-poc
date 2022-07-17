package co.topl.traffic.analyzer

import co.topl.traffic.Fixture
import co.topl.traffic.analyzer.TrafficAnalyzer.{Path, PathToTarget}
import co.topl.traffic.errors.TrafficAnalyzerInitError
import co.topl.traffic.model.cityNetwork.Intersection.IntersectionInterpolator
import co.topl.traffic.model.cityNetwork.{Intersection, RoadSegment}
import co.topl.traffic.parser.TrafficMeasurementsParser
import org.scalatest.OptionValues._
import org.scalatest.TryValues._
import org.scalatest.featurespec.AsyncFeatureSpec
import org.scalatest.matchers.should.Matchers

class TrafficAnalyzerSpec extends AsyncFeatureSpec with Matchers with Fixture {
  Feature("construction") {
    val somePathToTarget = List(i"A2" ~> target in 10)
    val pathToTarget = Map(target -> PathToTarget.init(target), i"A2" -> PathToTarget(List(i"A2" ~> target in 10, target ~> target in 0)))
    val minLeadingSegments = Map(target -> somePathToTarget)

    Scenario("PathToTarget with empty segments") {
      a[TrafficAnalyzerInitError] should be thrownBy PathToTarget(List.empty)
    }

    Scenario("PathToTarget.init must set an already shortest self path for the target") {
      PathToTarget.init(i"A1") shouldBe PathToTarget(List(i"A1" ~> i"A1" in 0), alreadyShortest = true)
    }

    Scenario("TrafficAnalyzer cannot be created if the target doesn't have at least one path leading to it") {
      TrafficAnalyzer(minLeadingSegments, target, target, pathToTarget).isSuccess shouldBe true
      TrafficAnalyzer(Map.empty, target, target, pathToTarget).failure.exception shouldBe a[TrafficAnalyzerInitError]
      TrafficAnalyzer(Map(target -> List.empty), target, target, pathToTarget).failure.exception shouldBe a[TrafficAnalyzerInitError]
      TrafficAnalyzer(Map(i"A2" -> somePathToTarget), target, target, pathToTarget).failure.exception shouldBe a[TrafficAnalyzerInitError]
    }

    Scenario("TrafficAnalyzer cannot be created if the current intersection doesn't have a calculated path to target") {
      TrafficAnalyzer(minLeadingSegments, target, i"A2", pathToTarget).isSuccess shouldBe true
      TrafficAnalyzer(minLeadingSegments, target, i"A2", Map.empty).failure.exception shouldBe a[TrafficAnalyzerInitError]
      TrafficAnalyzer(minLeadingSegments, target, i"A2", Map(target -> PathToTarget.init(target))).failure.exception shouldBe a[TrafficAnalyzerInitError]
    }

    Scenario("TrafficAnalyzer cannot be created if the self path to target is not flag as shortest") {
      TrafficAnalyzer(minLeadingSegments, target, i"A2", pathToTarget).isSuccess shouldBe true
      //noinspection RedundantDefaultArgument
      val notShortestSelfPath = PathToTarget(List(target ~> target in 0), alreadyShortest = false)
      TrafficAnalyzer(minLeadingSegments, target, i"A2", Map(target -> notShortestSelfPath)).failure.exception shouldBe a[TrafficAnalyzerInitError]
    }

    Scenario("TrafficAnalyzer infers the leadingSegments to each intersection from a set of segments") {
      TrafficAnalyzer.from(segments, target).success.value.leadingSegments shouldBe leadingSegments
    }
  }

  Feature("PathToTarget#>>:") {
    val origPath = PathToTarget.init(i"A1")

    Scenario("prepend a valid segment to an existent PathToTarget") {
      val newSegment = i"A2" ~> i"A1" in 12.123
      val newPath = newSegment >>: origPath

      newPath.start shouldBe newSegment.start
      newPath.segments shouldBe List(
        i"A2" ~> i"A1" in 12.123,
        i"A1" ~> i"A1" in 0,
      )
      newPath.alreadyShortest shouldBe false
    }

    Scenario("prepend an invalid segment to an existent PathToTarget does nothing") {
      val newSegment = i"A2" ~> i"A3" in 12.123
      (newSegment >>: origPath) shouldBe origPath
    }
  }

  Feature("PathToTarget#totalTransitTime") {
    Scenario("sum of all segment's transit times") {
      PathToTarget(List(
        i"B2" ~> i"B1" in 12.123,
        i"B1" ~> i"A1" in 123,
        i"A1" ~> i"A1" in 0,
      )).totalTransitTime shouldBe 135.123
    }
  }

  Feature("PathToTarget#shortest/shorterThan") {
    Scenario("path with less totalTransitTime is shorter") {
      val theShortest = PathToTarget(List(
        i"B2" ~> i"B1" in 3,
        i"B1" ~> i"A1" in 13,
        i"A1" ~> i"A1" in 0,
      ))

      val theLongest = PathToTarget(List(
        i"B2" ~> i"B1" in 12.123,
        i"B1" ~> i"A1" in 123,
        i"A1" ~> i"A1" in 0,
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
      analyzer.copy(pathToTarget = intersections.map(_ -> PathToTarget.init(target)).toMap).complete shouldBe true
    }
  }

  Feature("TrafficAnalyzer#selectNewCurrent") {
    Scenario("current intersection's path is marked as shortest and the new intersection is set as current") {
      analyzer.current shouldBe target
      analyzer.pathToTarget.contains(i"A2") shouldBe false
      analyzer.pathToTarget.contains(i"B3") shouldBe false

      val udpdatedAnalyzer = analyzer.selectNewCurrent(i"A2")

      udpdatedAnalyzer.current shouldBe i"A2"
      udpdatedAnalyzer.pathToTarget(i"A2").alreadyShortest shouldBe true
      udpdatedAnalyzer.pathToTarget(i"B3").alreadyShortest shouldBe false
    }
  }

  Feature("TrafficAnalyzer#nextCurrent") {
    Scenario("fails if pathToTarget is empty") {
      a[NoSuchElementException] should be thrownBy analyzer.copy(pathToTarget = Map.empty).nextCurrent
    }

    Scenario("None if pathToTarget only have shortest paths") {
      val someSegment = target ~> target in 0
      analyzer.copy(pathToTarget = intersections.map(_ -> PathToTarget(List(someSegment), alreadyShortest = true)).toMap).nextCurrent shouldBe None
    }

    Scenario("return the shortest of all non shortest paths") {
      val pathToTarget = Map(
        target -> PathToTarget(List(target ~> target in 0), alreadyShortest = true),
        i"A2" -> PathToTarget(List(i"A2" ~> target in 4, target ~> target in 0), alreadyShortest = true),
        i"B3" -> PathToTarget(List(i"B3" ~> target in 6.1, target ~> target in 0)), // 6.1
        i"A1" -> PathToTarget(List(i"A1" ~> i"A2" in 2, i"A2" ~> target in 4, target ~> target in 0)), // 6, shortest than B3
      )
      analyzer.copy(leadingSegments = leadingSegments, current = i"A2", pathToTarget = pathToTarget).nextCurrent shouldBe Some(i"A1")
    }
  }

  Feature("TrafficAnalyzer#updatedPathsViaCurrent") {
    Scenario("it should add paths to intersections leading to current on each iteration") {
      // pre conditions
      leadingSegments(i"A2") shouldBe List(i"A1" ~> i"A2" in 2)
      leadingSegments(target) shouldBe List(i"A2" ~> target in 4, i"B3" ~> target in 6.1)

      // initial state
      analyzer.pathToTarget shouldBe Map(target -> PathToTarget.init(target))

      // first iteration with current = target
      analyzer.nextCurrent shouldBe Some(i"A2") // because A2 -> target [4] vs B3 -> target [6]
      analyzer.updatedPathsViaCurrent shouldBe Map(
        target -> PathToTarget(List(target ~> target in 0), alreadyShortest = true),
        i"A2" -> PathToTarget(List(i"A2" ~> target in 4, target ~> target in 0)),
        i"B3" -> PathToTarget(List(i"B3" ~> target in 6.1, target ~> target in 0)),
      )

      // second iteration with current = A2
      val secondIteration = analyzer.selectNewCurrent(i"A2")
      secondIteration.nextCurrent shouldBe Some(i"A1") // because A1 -> A2 -> target [6] vs B3 -> target [6.1]
      secondIteration.updatedPathsViaCurrent shouldBe Map(
        target -> PathToTarget(List(target ~> target in 0), alreadyShortest = true),
        i"A2" -> PathToTarget(List(i"A2" ~> target in 4, target ~> target in 0), alreadyShortest = true),
        i"B3" -> PathToTarget(List(i"B3" ~> target in 6.1, target ~> target in 0)),
        i"A1" -> PathToTarget(List(i"A1" ~> i"A2" in 2, i"A2" ~> target in 4, target ~> target in 0)),
      )

      // third iteration with current = A1
      val thirdIteration = secondIteration.selectNewCurrent(i"A1")
      thirdIteration.nextCurrent shouldBe Some(i"B3")
      thirdIteration.updatedPathsViaCurrent should contain allElementsOf Map(
        target -> PathToTarget(List(target ~> target in 0), alreadyShortest = true),
        i"A2" -> PathToTarget(List(i"A2" ~> target in 4, target ~> target in 0), alreadyShortest = true),
        i"B3" -> PathToTarget(List(i"B3" ~> target in 6.1, target ~> target in 0)),
        i"A1" -> PathToTarget(List(i"A1" ~> i"A2" in 2, i"A2" ~> target in 4, target ~> target in 0), alreadyShortest = true),
      )
    }
  }

  Feature("TrafficAnalyzer#shortestPathFrom") {
    Scenario("it should find the shortest path from source to target and stops when it finds it") {
      val r = analyzer._shortestPathFrom(i"B1")
      r.current shouldBe i"B1"
      r.nextCurrent shouldNot be(None)
      analyzer.shortestPathFrom(i"B1") shouldBe Some(7, List(i"B1" ~> i"A1" in 1, i"A1" ~> i"A2" in 2, i"A2" ~> i"B2" in 4, i"B2" ~> i"B2" in 0))
    }

    Scenario("it should return None if the target is unreacheable from the source") {
      val inexistentIntersection = i"Z99"
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

  // TODO: It shouldn't be here with the unit tests, move
  Feature("Integration Test") {
    Scenario("Shortest path between C11 -> K14") {
      val segments: Set[RoadSegment] = TrafficMeasurementsParser("file:dist/sample-data.json").roadSegments().success.value

      // pretty similar paths, time difference is too close to guess manually so we try the four of them to check we actually expect the shortest one
      val possiblePaths: Seq[Seq[Intersection]] = Seq(
        Seq(i"C11", i"D11", i"E11", i"F11", i"F12", i"F13", i"F14", i"F15", i"G15", i"H15", i"I15", i"J15", i"K15", i"K14"), // this one should be the shortest
        Seq(i"C11", i"D11", i"E11", i"F11", i"G11", i"G12", i"G13", i"G14", i"G15", i"H15", i"I15", i"J15", i"K15", i"K14"),
        Seq(i"C11", i"D11", i"E11", i"F11", i"G11", i"H11", i"H12", i"H13", i"H14", i"H15", i"I15", i"J15", i"K15", i"K14"),
        Seq(i"C11", i"D11", i"E11", i"F11", i"G11", i"H11", i"I11", i"I12", i"I13", i"I14", i"I15", i"J15", i"K15", i"K14")
      )

      // transform to Path with the total transit time
      val possiblePathsWithTime: Seq[(Path, Double)] = possiblePaths.map {
        case i1 :: i2 :: is => is.scanLeft(i1 ~> i2)(_.end ~> _).map(unmeasured => segments.find(_.unmeasured == unmeasured).value)
      }.map(path => path -> path.map(_.transitTime).sum)

      val expectedPath = List(
        i"C11" ~> i"D11" in 25.51417201990442,
        i"D11" ~> i"E11" in 39.295753490989384,
        i"E11" ~> i"F11" in 43.32949273004243,
        i"F11" ~> i"F12" in 52.75998329436349,
        i"F12" ~> i"F13" in 18.09787398279722,
        i"F13" ~> i"F14" in 19.33401267736381,
        i"F14" ~> i"F15" in 35.09447113215229,
        i"F15" ~> i"G15" in 74.84999623543894,
        i"G15" ~> i"H15" in 49.44891438473805,
        i"H15" ~> i"I15" in 24.692971197960265,
        i"I15" ~> i"J15" in 33.19048405763468,
        i"J15" ~> i"K15" in 19.571258985533472,
        i"K15" ~> i"K14" in 17.056061491788984,
        i"K14" ~> i"K14" in 0,
      )

      possiblePathsWithTime.minBy(_._2)._1 shouldBe expectedPath.dropRight(1) // check the calculated min path is the same we believe it is
      TrafficAnalyzer.from(segments, i"K14").success.value.shortestPathFrom(i"C11") shouldBe Some(452.23544568070736, expectedPath)
    }
  }
}