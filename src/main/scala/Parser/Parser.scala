package Parser

trait Parser[OUT, IN] {

  def apply(s: State[IN]): OUT = {
    ask(s) match {
      case Right(result) => result
      case Left(error) => throw error
    }
  }

  def ask(s: State[IN]): Either[Exception, OUT]

  def ask(s: Seq[IN]): Either[Exception, OUT] = ask(State(s))

  def option(s: State[IN]): Option[OUT] = {
    try {
      Some(apply(s))
    } catch {
      case _: Exception =>
        Option.empty[OUT]
    }
  }

  def >>[O](p: Parser[O, IN]): Parser[O, IN] = (s: State[IN]) => {
    this ask s flatMap {_ => p ask s}
  }

  def <|>(p: Parser[OUT, IN]): Parser[OUT, IN] = new Choice((Seq(this, p)))
}
