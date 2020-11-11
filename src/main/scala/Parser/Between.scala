package Parser

class Between[OUT, IN](val open: Parser[_, IN], val close: Parser[_, IN], val p: Parser[OUT, IN]) extends  Parser[OUT, IN] {
  override def ask(s: State[IN]): Either[Exception, OUT] = {
    for {
      _ <- open ask s
      re <- p ask s
      _ <- close ask s
    } yield re
  }
}

object Between {
  def apply[OUT, IN](open: Parser[_, IN], close: Parser[_, IN], p: Parser[OUT, IN]): Between[OUT, IN] = new Between(open, close, p)
}
