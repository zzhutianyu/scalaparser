package json.parser

import Parser.{Parser, State, Text}
import json.{JNull, Json}

class N extends Parser[Json, Char]{
  override def ask(s: State[Char]): Either[Exception, Json] = {
    val p = new Text("null", true)
    p ask s map {_ => JNull()}
  }
}
