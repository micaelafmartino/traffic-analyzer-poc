package co.topl.traffic.analyzer

import co.topl.traffic.analyzer.TrafficAnalyzer._
import co.topl.traffic.errors.TrafficAnalyzerInitError
import co.topl.traffic.model.cityNetwork.{Intersection, RoadSegment}

import scala.annotation.tailrec
import scala.util.Try

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
  /** Indicates if the analyzer has already calculated the best routes for each intersection */
  lazy val complete = leadingSegments.keySet == pathToTarget.filter(_._2.alreadyShortest).keySet

  /** Shortest path from every intersection to [[target]] */
  def shortestPaths(): TrafficAnalyzer = run(_.complete)

  /** Shortest path from the given source intersection to [[target]]. None means the target intersection is unreacheable from source */
  def shortestPathFrom(source: Intersection): Option[(Double, Path)] = _shortestPathFrom(source).pathToTarget.get(source).map(path => (path.totalTransitTime, path.segments))

  /** Just here for testing purposes */
  private[analyzer] def _shortestPathFrom(source: Intersection): TrafficAnalyzer = run(_.current == source)

  @tailrec // for this to be tailrec we cannot map+getOrElse/fold over newCurrent, so isEmpty+get it is..
  private def run(exitCondition: TrafficAnalyzer => Boolean): TrafficAnalyzer = if (exitCondition(this) || nextCurrent.isEmpty) this else selectNewCurrent(nextCurrent.get).run(exitCondition)

  /** Marks [[current]] as visited and selects a new [[current]] intersection */
  private[analyzer] def selectNewCurrent(newCurrent: Intersection): TrafficAnalyzer = copy(
    current = newCurrent,
    pathToTarget = updatedPathsViaCurrent.updatedWith(newCurrent)(_.map(_.copy(alreadyShortest = true)))
  )

  /** Non-visited (aka shortest path not found yet) intersection with the shortest distance to the target so far. None means there is no other intersection to analyze */
  private[analyzer] lazy val nextCurrent: Option[Intersection] = updatedPathsViaCurrent.filterNot(_._2.alreadyShortest).minByOption(_._2.totalTransitTime).map(_._1)

  /** Updated [[pathToTarget]] with new/better paths to the target via the current intersection from all it's leading segments */
  private[analyzer] lazy val updatedPathsViaCurrent: Map[Intersection, PathToTarget] = leadingSegments(current).foldLeft(pathToTarget) { (pathToTarget, leadingSegment) =>
    val pathViaCurrent = leadingSegment >>: pathToTarget(current)
    pathToTarget.updatedWith(leadingSegment.start)(_.orElse(Some(pathViaCurrent)).map(_ shortest pathViaCurrent))
  }
}

object TrafficAnalyzer {
  /** Ordered sequence of road segments to travel from one intersection to another */
  type Path = List[RoadSegment]

  /** [[Map]] with the leading segments to each existing intersection */
  type LeadingSegments = Map[Intersection, Path]

  def from(segments: Set[RoadSegment], target: Intersection): Try[TrafficAnalyzer] = TrafficAnalyzer(segments.toList.groupBy(_.end), target)

  def apply(leadingSegments: LeadingSegments, target: Intersection): Try[TrafficAnalyzer] = TrafficAnalyzer(
    leadingSegments = leadingSegments,
    target = target,
    current = target,
    pathToTarget = Map(target -> PathToTarget.init(target))
  )

  def apply(leadingSegments: LeadingSegments, target: Intersection, current: Intersection, pathToTarget: Map[Intersection, PathToTarget]): Try[TrafficAnalyzer] = Try {
    if (!leadingSegments.get(target).exists(_.nonEmpty)) throw TrafficAnalyzerInitError(s"Target $target is unreacheable (no segment road leads to it)")
    if (!pathToTarget.contains(current)) throw TrafficAnalyzerInitError(s"Invalid state: there must always be an already calculated path from current $current to target $target")
    if (!pathToTarget.get(target).exists(_.alreadyShortest)) throw TrafficAnalyzerInitError(s"Self path from/to target $target not present in pathToTarget (PathToTarget([$target -> $target], alreadyShortest = true))")

    new TrafficAnalyzer(leadingSegments, target, current, pathToTarget)
  }

  /**
   * Util class to handle paths in the analyzer.
   *
   * @param alreadyShortest flags if the path denoted by [[segments]] is the shortest one between those start and ending interesections
   * @param segments        a sequence of [[RoadSegment]] of which this path is composed. The [[RoadSegment.start]] intersection of the first segment is the
   *                        source and the [[RoadSegment.end]] intersection of the last segment is the target.
   */
  case class PathToTarget(segments: Path, alreadyShortest: Boolean) {
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

  object PathToTarget {
    def init(target: Intersection): PathToTarget = PathToTarget(segments = List(RoadSegment(0, target, target)), alreadyShortest = true)

    def apply(segments: Path, alreadyShortest: Boolean = false): PathToTarget =
      if (segments.nonEmpty) new PathToTarget(segments, alreadyShortest) else throw TrafficAnalyzerInitError("PathToTarget needs at least one segment from target to target to be initialized")
  }
}