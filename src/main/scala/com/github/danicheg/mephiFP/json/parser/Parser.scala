package com.github.danicheg.mephiFP.json.parser

import com.github.danicheg.mephiFP.json._

import scala.annotation.tailrec

object Parser {

    sealed trait Token
    case object OpenSB extends Token
    case object OpenCB extends Token
    case object CloseCB extends Token
    case object CloseSB extends Token
    case object Colon extends Token
    case object Comma extends Token
    case object NullToken extends Token
    case class StringToken(str: String) extends Token
    case class NumberToken(num: Double) extends Token
    case class BooleanToken(value: Boolean) extends Token

    def toToken(input: String): List[Token] = {

        @tailrec
        def parseStr(rest: List[Char], acc: List[Char] = Nil): (List[Char], List[Char]) = rest match {
            case '\\' :: '"' :: xs => parseStr(xs, acc ::: List('\\', '"'))
            case '\\' :: 'n' :: xs => parseStr(xs, acc ::: List('\\', 'n'))
            case '"' :: xs => (xs, acc)
            case x :: xs => parseStr(xs, acc ::: List(x))
            case Nil => (Nil, acc)
        }

        @tailrec
        def parseNum(rest: List[Char], acc: List[Char] = Nil): (List[Char], List[Char]) = rest match {
            case x :: xs =>
                if (Set(':', ',', ')', ' ', ']').contains(x)) (xs, acc)
                else parseNum(xs, acc ::: List(x))
            case Nil => (Nil, acc)
        }

        @tailrec
        def loop(rest: List[Char], acc: List[Token] = Nil): List[Token] = rest match {
            case x :: xs if x.isWhitespace => loop(xs, acc)
            case '{' :: xs => loop(xs, acc ::: List(OpenCB))
            case '}' :: xs => loop(xs, acc ::: List(CloseCB))
            case '[' :: xs => loop(xs, acc ::: List(OpenSB))
            case ']' :: xs => loop(xs, acc ::: List(CloseSB))
            case ':' :: xs => loop(xs, acc ::: List(Colon))
            case ',' :: xs => loop(xs, acc ::: List(Comma))
            case '"' :: xs =>
                val (rst, str) = parseStr(xs)
                loop(rst, acc ::: List(StringToken(str.mkString)))
            case 'n' :: 'u' :: 'l' :: 'l' :: xs => loop(xs, acc ::: List(NullToken))
            case 't' :: 'r' :: 'u' :: 'e' :: xs => loop(xs, acc ::: List(BooleanToken(true)))
            case 'f' :: 'a' :: 'l' :: 's' :: 'e' :: xs => loop(xs, acc ::: List(BooleanToken(false)))
            case x :: xs if x.isDigit =>
                val (rst, num) = parseNum(xs, List(x.toChar))
                loop(rst, acc ::: List(NumberToken(num.mkString.toDouble)))
            case Nil => acc
        }

        loop(input.toList)

    }

    def fromToken(tokens: List[Token]): JsonValue = {

        def parse(tokens: List[Token]): (JsonValue, List[Token]) = {

            @tailrec
            def parseObj(rest: List[Token], obj: List[(String, JsonValue)] = Nil): (JsonValue, List[Token]) = rest match {
                case Comma :: xs => parseObj(xs, obj)
                case CloseCB :: xs => (JsonObject(obj.toMap), xs)
                case StringToken(str) :: Colon :: xs =>
                    val (js, rst) = parse(xs)
                    parseObj(rst, obj ::: List((str, js)))
            }

            @tailrec
            def parseArr(rest: List[Token], list: List[JsonValue] = Nil): (JsonValue, List[Token]) = rest match {
                case Comma :: xs => parseArr(xs, list)
                case CloseSB :: xs => (JsonArray(list), xs)
                case x :: xs =>
                    val (js, rst) = parse(x :: xs)
                    parseArr(rst, list ::: List(js))
            }

            tokens match {
                case OpenCB :: xs => parseObj(xs)
                case OpenSB :: xs => parseArr(xs)
                case NullToken :: xs => (JsonNull, xs)
                case StringToken(str) :: xs => (JsonString(str), xs)
                case NumberToken(num) :: xs => (JsonNumber(num), xs)
                case BooleanToken(value) :: xs => (JsonBoolean(value), xs)
            }
        }

        parse(tokens) match {
            case (res, Nil) => res
            case _ => sys.error(s"Problem with parsing tokens = $tokens")
        }
    }

    def toJson(str: String): JsonValue = fromToken(toToken(str))

    def fromJson(value: JsonValue): String = value match {
        case JsonObject(entries) =>
            val serializedEntries = for ((k,v) <- entries) yield s""""$k": ${fromJson(v)}"""
            s"{${serializedEntries.mkString(", ")}}"
        case JsonArray(entries) =>
            val serializedEntries = entries.map(fromJson)
            s"[${serializedEntries.mkString(", ")}]"
        case JsonString(str) => s""""$str""""
        case JsonNumber(num) => num.toString()
        case JsonBoolean(value) => value.toString
        case JsonNull => "null"
    }
}
