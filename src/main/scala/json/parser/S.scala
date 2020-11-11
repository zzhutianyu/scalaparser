package json.parser

import Parser.{Attempt, Between, Ch, Many, NotE, Parser, State}
import json.{JString, Json}

class S extends Parser[Json, Char] {
  val escape = new Escape
  val sP = new Parser[String, Char] {
    override def ask(s: State[Char]): Either[Exception, String] = {
      Attempt(escape) ask s match {
        case Right(v) => Right(v)
        case Left(e) => {
          NotE('"') ask s match {
            case Right(value) => Right(value.toString)
            case Left(err) => Left(err)
          }
        }
      }
    }
  }

  val nP: Between[Seq[String], Char] = Between(Ch('"'), Ch('"'), new Many(sP))

  override def ask(s: State[Char]): Either[Exception, Json] = {
    nP ask s map { value => new JString(value.mkString)}
  }
}
