package json.parser

import Parser.{Attempt, Parser, State, Text}
import json.{JBool, Json}

class B extends Parser[Json, Char] {

  override def ask(s: State[Char]): Either[Exception, Json] = {
    val trueP = new Text("true", true)
    val falseP = new Text("false", true)
    val nP = Attempt(trueP) <|> falseP
    nP ask s map {
      case "true" => JBool(true)
      case "false" => JBool(false)
    }
  }
}
