package com.github.danicheg.mephiFP.json

import parser.Parser._

object JsonWriter {

  def writeString(value: JsonValue): String = fromJson(value)

  def write[A](value: A)(implicit conv: JsonConverter[A]): String =
    writeString(conv.toJson(value))
}
