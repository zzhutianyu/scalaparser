package json.parser

import Parser._

class Escape extends Parser[String, Char] {

  val reverseSoliud = Ch('\\')
  val hexP =  Count(new Hex(), 4)
  val escCharP = Attempt(Attempt(Ch('"')) <|> Attempt(Ch('\\')) <|> Attempt(Ch('/')) <|> Attempt(Ch('b')) <|> Attempt(Ch('f')) <|> Attempt(Ch('n')) <|> Attempt(Ch('r')) <|> Ch('t'))
  override def ask(s: State[Char]): Either[Exception, String] = {
    reverseSoliud ask s match {
      case Left(error) => Left(error)
      case Right(_) => {
        escCharP ask s match {
          case Right(value) => Right(value.toString)
          case Left(err) => {
            Ch('u') ask s match {
              case Left(uErr) => Left(uErr)
              case Right(u) => {
                hexP ask s match {
                  case Left(e) => Left(e)
                  case Right(v) =>
                    Right(s"${Integer.parseInt(v.mkString, 16).toChar}")
                }
              }
            }
          }
        }
      }
    }
  }
}
