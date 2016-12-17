package com.github.danicheg.mephiFP.json.utils

import com.github.danicheg.mephiFP.json._

object count {
  def countElemsOfArrays(obj: JsonObject): Int = {

    def checkArray(list: List[JsonValue], counter: Int = 0): Int = list match {
      case JsonArray(_) :: xs => checkArray(xs, counter + 1)
      case y :: ys => checkArray(ys)
      case Nil => counter
    }

    def count(list: List[JsonValue] = Nil): Int =
      list match {
        case (x @ JsonObject(_)) :: xs => countElemsOfArrays(x) + count(xs)
        case (y @ JsonArray(_)) :: ys =>
          y.entries.length - checkArray(y.entries) +
            count(y.entries) + count(ys)
        case z :: zs => count(zs)
        case Nil => 0
      }

    count(obj.entries.values.toList)
  }
}
