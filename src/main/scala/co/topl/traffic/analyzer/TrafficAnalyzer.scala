package co.topl.traffic.analyzer

import co.topl.traffic.analyzer.TrafficAnalyzer._
import co.topl.traffic.model.cityNetwork.{Intersection, RoadSegment}

/** Looks up the shortest path to any intersection of the City's network using Dijkstra's shortest path algorithm
 *
 * @param leadingSegments [[Map]] with the leading segments to each existing intersection
 * @param target          ending intersection for which we are searching the shortest path
 * @param current         current intersection to be analyzed
 * @param pathToTarget    [[Map]] with all the paths already found from visited intersections and their adjacencies
 */
case class TrafficAnalyzer(leadingSegments: LeadingSegments,
                           target: Intersection,
                           current: Intersection,
                           pathToTarget: Map[Intersection, PathToTarget]) {
  def shortestPaths(): TrafficAnalyzer = if (leadingSegments.keySet == pathToTarget.filter(_._2.alreadyShortest).keySet) this else selectNext().shortestPaths()

  def shortestPathFrom(source: Intersection): (Intersection, Intersection, Double, Path) = pathToTarget
    .get(source)
    .filter(_.alreadyShortest)
    .fold(selectNext().shortestPathFrom(source))(path => (source, target, path.totalTransitTime, path.segments))

  /** Marks [[current]] as visited and selects a new [[current]] intersection */
  private def selectNext(): TrafficAnalyzer = copy(
    current = nextIntersection,
    pathToTarget = updatedPathsViaCurrent.updatedWith(current)(_.map(_.copy(alreadyShortest = true)))
  )

  /** Non-visited (aka shortest path not found yet) intersection with the shortest distance to the target so far */
  private lazy val nextIntersection: Intersection = updatedPathsViaCurrent.filterNot(_._2.alreadyShortest).minBy(_._2.totalTransitTime)._1

  /** Updated [[pathToTarget]] with new/better paths to the target via the current intersection from all it's leading segments */
  private lazy val updatedPathsViaCurrent: Map[Intersection, PathToTarget] = leadingSegments(current).foldLeft(pathToTarget) { (pathToTarget, leadingSegment) =>
    val pathViaCurrent = leadingSegment >>: pathToTarget(current)
    pathToTarget.updatedWith(leadingSegment.start)(_.orElse(Some(pathViaCurrent)).map(_ shortest pathViaCurrent))
  }
}

object TrafficAnalyzer {
  /** [[Map]] with the leading segments to each existing intersection */
  type LeadingSegments = Map[Intersection, Path]

  /** Ordered sequence of road segments to travel from one intersection to another */
  type Path = List[RoadSegment]

  def apply(leadingSegments: LeadingSegments, target: Intersection): TrafficAnalyzer = new TrafficAnalyzer(
    leadingSegments = leadingSegments,
    target = target,
    current = target,
    pathToTarget = Map(target -> PathToTarget(segments = List(RoadSegment(0, target, target)), alreadyShortest = true))
  )

  /**
   * Util class to handle paths in the analyzer.
   *
   * @param alreadyShortest flags if the path denoted by [[segments]] is the shortest one between those start and ending interesections
   * @param segments        a sequence of [[RoadSegment]] of which this path is composed. The [[RoadSegment.start]] intersection of the first segment is the
   *                        source and the [[RoadSegment.end]] intersection of the last segment is the target.
   */
  case class PathToTarget(segments: Path, alreadyShortest: Boolean = false) {
    /** Intersection from which this Path starts (the start of the first segment from [[segments]]) */
    lazy val start: Intersection = segments.head.start

    /** The sum of the transit times for all [[segments]] */
    lazy val totalTransitTime: Double = segments.map(_.transitTime).sum

    /** Returns the shortest Path between this and other given path */
    def shortest(otherPath: PathToTarget): PathToTarget = if (shorterThan(otherPath)) this else otherPath

    def shorterThan(otherPath: PathToTarget): Boolean = totalTransitTime < otherPath.totalTransitTime

    /** Prepends a valid leading road segment to this Path.
     *
     * In case the given segment does not lead to the start of this path, nothing is added and this path is returned.
     * */
    def >>:(leadingSegment: RoadSegment): PathToTarget = if (leadingSegment.end != start) this else PathToTarget(segments = leadingSegment +: segments)
  }
}