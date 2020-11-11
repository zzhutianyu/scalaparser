package Parser

class SkipSpaces extends Parser[Unit, Char] {
  val parser = Skip[Char](Space())

  override def ask(s: State[Char]): Either[Exception, Unit] = parser ask s
}

object SkipSpaces{
  def apply(): SkipSpaces = new SkipSpaces()
}
