package Parser

class Skip1[IN](val parser: Parser[_, IN]) extends Parser[Unit, IN] {
  val skip = Skip(parser)
  val psr = parser >> skip

  override def ask(s: State[IN]): Either[Exception, Unit] = psr ask s
}

object Skip1 {
  def apply[IN](parser: Parser[_, IN]): Skip1[IN] = new Skip1(parser)
}
