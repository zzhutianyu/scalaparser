package Parser

class SkipWhiteSpaces extends Parser[Unit, Char] {
  val parser = Skip[Char](WhiteSpace())

  override def ask(s: State[Char]): Either[Exception, Unit] = parser ask s
}
