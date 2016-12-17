package com.github.danicheg.mephiFP.json

trait JsonConverter[A] {
  def toJson(value: A): JsonValue
  def fromJson(value: JsonValue): A
}
