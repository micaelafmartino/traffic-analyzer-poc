package co.topl.traffic.utils

import play.api.libs.json.{JsResultException, JsValue, Json, Reads}

import scala.util.{Failure, Success, Try}

package object extensions {
  implicit class JsValueExtensions(json: JsValue) {
    /** Tries to convert the node into a T, returning a [[Failure]] with the exception if it can't */
    def asTry[T: Reads]: Try[T] = json.validate[T].fold(valid = Success(_), invalid = e => Failure(JsResultException(e)))

    /** Shortcut for [[Json.prettyPrint]] */
    def prettyString(): String = Json.prettyPrint(json)
  }
}
