package json.parser

import Parser.{Ahead, Attempt, Between, Ch, Many, Parser, SkipSpaces, SkipWhiteSpaces, State, WhiteSpace}
import json.{JObject, JString, Json}

import scala.annotation.tailrec

class O extends Parser[Json, Char] {

  val skips = new SkipWhiteSpaces()
  lazy val p = new JsonParser
  val kvP = new Parser[Map[JString, Json], Char] {
    val end = new Ahead(skips >> Ch('}'))
    override def ask(s: State[Char]): Either[Exception, Map[JString, Json]] = {
      val sP = new S
      @tailrec
      @inline
      def loop(next: Map[JString, Json]): Either[Exception, Map[JString, Json]] = {
        val transaction = s.begin()
        Attempt(Ch(',')) ask s
        val res = for {
          _ <- skips ask s
          str <- sP ask s
          _ <- skips ask s
          _ <- Ch(':') ask s
          _ <- skips ask s
          js <- p ask s
          _ <- skips ask s
        } yield (str.asInstanceOf[JString] -> js)

        res match {
          case Left(error) => {
            s rollback transaction
            Right(next)
          }
          case Right(value) => loop(next + value)
        }
      }
      val res = loop(Map())
      end ask s match {
        case Left(e) => Left(e)
        case Right(_) => res
      }
    }
  }

  override def ask(s: State[Char]): Either[Exception, Json] = {
    val nP = Between(Ch('{') >> skips, skips >> Ch('}') , Attempt(kvP))
    nP ask s map(JObject(_))
  }
}
