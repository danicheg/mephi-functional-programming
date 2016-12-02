package com.github.danicheg.mephiFP.json

import org.specs2.mutable.Specification

import com.github.danicheg.mephiFP.json._

case class A(price: Double, name: String)

class JsonSpec extends Specification {

    "Json mapper" should {

        implicit val jsonConverterA = new JsonConverter[A] {
            def toJson(value: A): JsonValue =
                JsonObject(Map("price" -> JsonNumber(value.price), "name" -> JsonString(value.name)))

            def fromJson(value: JsonValue): A = value match {
                case JsonObject(objs) => A(objs("price").asInstanceOf[JsonNumber].num,
                    objs("name").asInstanceOf[JsonString].str)
            }
        }

        val json = s"""{"price": 51.0, "name": "book"}"""

        val instance = A(51.0, "book")

        "generate correct instance of case class from json" in {
            JsonReader.read(json) must beEqualTo(instance)
        }

        "generate correct json from instance json" in {
            JsonWriter.write(instance) must beEqualTo(json)
        }
    }
}
