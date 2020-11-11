package Parser

class Space extends Parser [Char, Char] {

  override def ask(s: State[Char]): Either[Exception, Char] = {
    s.next() flatMap {re =>
      if (re.isSpaceChar) Right(re)
      else Left(new ParserException(s.status, s"Expect $re is space."))
    }
  }
}

object Space {
  def apply(): Space = new Space()
}
