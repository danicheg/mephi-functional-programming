package com.github.danicheg.mephiFP.json

import parser.Parser._

object JsonReader {

  def readString(value: String): JsonValue = toJson(value)

  def read[A](value: String)(implicit conv: JsonConverter[A]): A = {
    conv.fromJson(readString(value))
  }
}
