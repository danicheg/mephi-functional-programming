package com.github.danicheg.mephiFP.json

object JsonWriter {
    def write(value: JsonValue): String = value match {
        case JsonObject(entries) => {
            val serializedEntries = for ((k,v) <- entries) yield s"$k: ${write(value)}"
            s"{ ${serializedEntries.mkString(", ")} }"
        }
        case JsonArray(entries) => {
            val serializedEntries = entries.map(write)
            s"[ ${serializedEntries.mkString(", ")} ]"
        }
        case JsonString(str) => s"\"$str\""
        case JsonNumber(num) => num.toString()
        case JsonBoolean(value) => value.toString
        case JsonNull => "null"
    }

    def write[A](value: A)(conv: JsonConverter[A]): String =
        write(conv.toJson(value))
}
