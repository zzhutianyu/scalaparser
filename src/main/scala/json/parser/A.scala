package json.parser

import Parser.{Attempt, Between, Ch, Many, Parser, SkipWhiteSpaces, State}
import json.{JArray, Json}

class A extends Parser[Json, Char]{

  lazy val p = new JsonParser
  val skips = new SkipWhiteSpaces
  val valueP = new Parser[Json, Char] {
    val end = skips >> Ch(']')
    override def ask(s: State[Char]): Either[Exception,Json] = {
      val transaction = s.begin()
      Attempt(Ch(',')) ask s
      val res = for {
        json <- p ask s
      } yield json
      res match {
        case Left(e) =>
          s rollback transaction
          Left(e)
        case Right(v) => Right(v)
      }

    }
  }
  override def ask(s: State[Char]): Either[Exception, Json] = {
    val nP = Between(skips >> Ch('['), skips >> Ch(']') >> skips, new Many(valueP))
    nP ask s map(JArray(_))
  }
}
