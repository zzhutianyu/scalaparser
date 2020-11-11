package Parser

class Digit extends Parser[Char, Char] {
  override def ask(s: State[Char]): Either[Exception, Char] = {
    s.next() flatMap {re =>
      if (re.isDigit) Right(re)
      else Left(new ParserException(s.status, s"Expect $re is digit."))
    }
  }
}

object Digit {
  def apply(): Digit = new Digit()
}
