package com.github.danicheg.mephiFP.json.utils

import com.github.danicheg.mephiFP.json._

import scala.collection.immutable.NumericRange

object random {

  def genString(): JsonString = {
    def eval(n: Int, list: List[Char] = Nil): List[Char] =
      if (n == 0) util.Random.nextPrintableChar() :: list
      else eval(n - 1, util.Random.nextPrintableChar() :: list)

    JsonString(eval(util.Random.nextInt(50)).mkString)
  }

  def genNumber(): JsonNumber =
    JsonNumber(util.Random.nextDouble() * util.Random.nextInt())

  def genBoolean(): JsonBoolean =
    JsonBoolean(System.currentTimeMillis() % 2 == 0)

  def genArray(): JsonArray = {
    def eval(n: Int, list: List[JsonValue] = Nil): List[JsonValue] =
      if (n == 0) list
      else {
        val randomList = List(JsonNull,
                              JsonNumber,
                              JsonBoolean,
                              JsonString,
                              JsonArray,
                              JsonObject)
        val index = util.Random.nextInt(5)
        randomList(index) match {
          case JsonNull => eval(n - 1, JsonNull :: list)
          case JsonNumber => eval(n - 1, genNumber() :: list)
          case JsonBoolean => eval(n - 1, genBoolean() :: list)
          case JsonString => eval(n - 1, genString() :: list)
          case JsonArray => eval(n - 1, genArray() :: list)
          case JsonObject => eval(n - 1, list)
        }
      }

    JsonArray(eval(util.Random.nextInt(10)))
  }

  def genObject(): JsonObject = {
    def genKey(): String = {
      def eval(n: Int, list: List[Char] = Nil): List[Char] =
        if (n == 0) list
        else eval(n - 1, util.Random.alphanumeric.head :: list)
      eval(util.Random.nextInt(32)).mkString
    }

    def eval(n: Int,
             fields: Map[String, JsonValue] = Map()): Map[String, JsonValue] =
      if (n == 0) fields
      else {
        val randomList = List(JsonNull,
                              JsonNumber,
                              JsonBoolean,
                              JsonString,
                              JsonArray,
                              JsonObject)
        val index = util.Random.nextInt(5)
        randomList(index) match {
          case JsonNull => eval(n - 1, Map(genKey() -> JsonNull) ++ fields)
          case JsonNumber =>
            eval(n - 1, Map(genKey() -> genNumber()) ++ fields)
          case JsonBoolean =>
            eval(n - 1, Map(genKey() -> genBoolean()) ++ fields)
          case JsonString =>
            eval(n - 1, Map(genKey() -> genString()) ++ fields)
          case JsonArray => eval(n - 1, Map(genKey() -> genArray()) ++ fields)
          case JsonObject => eval(n - 1, Map(genKey() -> JsonNull) ++ fields)
        }
      }

    JsonObject(eval(util.Random.nextInt(32)))
  }
}
