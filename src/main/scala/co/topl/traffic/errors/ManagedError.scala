package co.topl.traffic.errors

import co.topl.traffic.model.cityNetwork.Intersection

sealed trait ManagedError extends RuntimeException

case class UserInputParseError(inputName: String, input: String, cause: Throwable) extends ManagedError

case class TrafficDataParseError(cause: Throwable) extends ManagedError

case class NoRouteFound(source: Intersection, target: Intersection) extends ManagedError

case class TrafficAnalyzerInitError(detail: String, cause: Option[Throwable] = None) extends RuntimeException(detail, cause.orNull) with ManagedError