package Parser

import scala.annotation.tailrec

class Choice[OUT, IN](val parsers: Seq[Parser[OUT, IN]]) extends Parser[OUT, IN] {
  override def ask(s: State[IN]): Either[Exception, OUT] = {
    val status = s.status
    var error: Option[Exception] = None
    for(parser <- parsers) {
      parser ask s match {
        case right: Right[Exception, OUT] =>
          return right
        case Left(err) =>
          error = Some(err)
          if (s.status != status) return Left(err)
      }
    }
    error match {
      case Some(err) => Left(new ParserException(status, "failed"))
      case None => Left(new ParserException(status, "all parsers failed"))
    }
  }
}
