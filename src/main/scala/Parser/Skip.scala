package Parser

import scala.annotation.tailrec

class Skip[IN](val parser: Parser[_, IN]) extends Parser [Unit, IN] {
  val p: Attempt[_, IN] =  Attempt(parser)

  override def ask(s: State[IN]): Either[Exception, Unit] = {
    while (true) {
      val res = p ask s
      if (res.isLeft) return Right()
    }
    Right()
  }
}

object Skip {
  def apply[IN](parser: Parser[_, IN]): Skip[IN] = new Skip(parser)
}
