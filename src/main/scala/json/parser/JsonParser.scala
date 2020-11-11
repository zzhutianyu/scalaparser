package json.parser

import Parser.{Attempt, Between, Parser, SkipSpaces, SkipWhiteSpaces, State}
import json.Json

class JsonParser extends Parser[Json, Char] {
  val skips = new SkipWhiteSpaces()
  override def ask(s: State[Char]): Either[Exception, Json] = {
    val p = Attempt(Attempt(new S) <|> Attempt(new A) <|> Attempt(new N) <|> Attempt(new B) <|> new O())
    val nP = Between(skips, skips, p)
    nP ask s
  }
}
