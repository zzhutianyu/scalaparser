package Parser

class Attempt[OUT, IN](val parser: Parser[OUT, IN]) extends Parser[OUT, IN] {
  override def ask(s: State[IN]): Either[Exception, OUT] = {
    val transaction = s.begin()
    parser ask s match {
      case right: Right[_, _] =>
        s commit transaction
        right
      case left: Left[_, _] =>
        s rollback transaction
        left
    }
  }
}

object Attempt {
  def apply[OUT, IN](parser: Parser[OUT, IN]): Attempt[OUT, IN] = new Attempt(parser)
}
