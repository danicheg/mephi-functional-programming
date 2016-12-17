package com.github.danicheg.mephiFP.json

import org.specs2.mutable.Specification

import com.github.danicheg.mephiFP.json._

import com.github.danicheg.mephiFP.json.utils._

case class Stuff(price: Double, name: String)
case class User(firstName: String, lastName: String)
case class TableSchema(user: User, stuff: Stuff)

class JsonSpec extends Specification {

  "Json mapper" should {

    implicit val jsonConverterUser = new JsonConverter[User] {
      def toJson(user: User): JsonValue =
        obj(
          "firstName" -> JsonString(user.firstName),
          "lastName" -> JsonString(user.lastName)
        )

      def fromJson(json: JsonValue): User = json match {
        case JsonObject(fields) =>
          val firstName = fields get "firstName" match {
            case Some(JsonString(str)) => str
          }
          val lastName = fields get "lastName" match {
            case Some(JsonString(str)) => str
          }
          User(firstName, lastName)
      }
    }

    implicit val jsonConverterStuff = new JsonConverter[Stuff] {
      def toJson(stuff: Stuff): JsonValue =
        obj("price" -> JsonNumber(stuff.price),
            "name" -> JsonString(stuff.name))

      def fromJson(stuff: JsonValue): Stuff = stuff match {
        case JsonObject(fields) =>
          val price = fields get "price" match {
            case Some(JsonNumber(num)) => num
          }
          val name = fields get "name" match {
            case Some(JsonString(str)) => str
          }
          Stuff(price, name)
      }
    }

    implicit val jsonConverterTableSchema = new JsonConverter[TableSchema] {
      def toJson(schema: TableSchema): JsonValue =
        obj(
          "user" -> obj("firstName" -> JsonString(schema.user.firstName),
                        "lastName" -> JsonString(schema.user.lastName)),
          "stuff" -> obj("price" -> JsonNumber(schema.stuff.price),
                         "name" -> JsonString(schema.stuff.name))
        )

      def fromJson(json: JsonValue): TableSchema = json match {
        case JsonObject(fields) =>
          val user = fields get "user" match {
            case Some(JsonObject(userObj)) =>
              val firstName = userObj get "firstName" match {
                case Some(JsonString(str)) => str
              }
              val lastName = userObj get "lastName" match {
                case Some(JsonString(str)) => str
              }
              User(firstName, lastName)
          }
          val stuff = fields get "stuff" match {
            case Some(JsonObject(stuffObj)) =>
              val price = stuffObj get "price" match {
                case Some(JsonNumber(num)) => num
              }
              val name = stuffObj get "name" match {
                case Some(JsonString(str)) => str
              }
              Stuff(price, name)
          }
          TableSchema(user, stuff)
      }
    }

    val jsonStuff = s"""{"price": 51.0, "name": "book"}"""

    val stuff = Stuff(51.0, "book")

    val user = User("Erlich", "Bachman")

    val jsonTableSchema =
      s"""{"user": {"firstName": "Erlich", "lastName": "Bachman"}, "stuff": {"price": 51.0, "name": "book"}}"""

    val buy = TableSchema(user, stuff)

    "generate correct instance of case class Stuff from Json" in {
      JsonReader.read[Stuff](jsonStuff) must beEqualTo(stuff)
    }

    "generate correct json from instance class Stuff" in {
      JsonWriter.write(stuff) must beEqualTo(jsonStuff)
    }

    "generate correct instance of case class TableSchema from Json" in {
      JsonReader.read[TableSchema](jsonTableSchema) must beEqualTo(buy)
    }

    "generate correct json from instance class TableSchema" in {
      JsonWriter.write(buy) must beEqualTo(jsonTableSchema)
    }

    "generate random Json string" in {
      random.genString() must not(beEqualTo(random.genString()))
    }

    "generate random Json number" in {
      random.genNumber() must not(beEqualTo(random.genNumber()))
    }

    "generate random Json array" in {
      random.genArray() must not(beEqualTo(random.genArray()))
    }

    "generate random Json object" in {
      random.genObject() must not(beEqualTo(random.genObject()))
    }

    "correct count number of elements of all Json arrays in base case" in {
      val jsObj = obj("a" -> JsonNull)

      count.countElemsOfArrays(jsObj) must beEqualTo(0)
    }

    "correct count number of elements of all Json arrays in arbitrary case" in {
      val jsObj =
        obj("a" -> arr(JsonString("sd"),
                       JsonNumber(23.2),
                       arr(JsonString("zsd"), JsonNumber(21.2))),
            "z" -> arr(JsonString("szd"),
                       JsonNumber(223.2),
                       arr(JsonString("zssd"), JsonNumber(212.2))),
            "bar" -> obj("d" -> JsonString("qwer"),
                         "qq" -> JsonBoolean(true),
                         "p" -> obj("foo" -> arr(JsonString("bar")))))

      count.countElemsOfArrays(jsObj) must beEqualTo(9)
    }

    "correct count number of elements of all arrays in random generated Json" in {
      val json = random.genObject()

      count.countElemsOfArrays(json) must not(beEqualTo(0))
    }
  }
}
