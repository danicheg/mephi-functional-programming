package com.github.danicheg.mephiFP

package object json {

  sealed trait JsonValue

  case class JsonObject(entries: Map[String, JsonValue]) extends JsonValue

  case class JsonArray(entries: List[JsonValue]) extends JsonValue

  case class JsonString(str: String) extends JsonValue

  case class JsonNumber(num: Double) extends JsonValue

  case class JsonBoolean(value: Boolean) extends JsonValue

  case object JsonNull extends JsonValue

  def obj(fields: (String, JsonValue)*): JsonObject =
    JsonObject(fields.toMap)

  def arr(fields: JsonValue*): JsonArray =
    JsonArray(fields.toList)

}
