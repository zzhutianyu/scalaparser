package Parser

class Ch(val char: Char, val caseSensitive: Boolean) extends Parser[Char, Char] {
  val chr: Char = if (caseSensitive) char else char.toLower

  override def ask(s: State[Char]): Either[Exception, Char] = {
    s.next() flatMap { c=>
      if (caseSensitive) {
        if (chr == c) {
          return Right(c)
        }
      } else {
        if (chr == c.toLower) {
          return Right(c)
        }
      }
      Left(new ParserException(s.status, s"expect char $char (case senstitive $caseSensitive) but get $c"))
    }
  }
}

object Ch {
  def apply(char: Char, caseSensitive: Boolean): Ch = new Ch(char, caseSensitive)

  def apply(char: Char): Ch = new Ch(char, true)
}
