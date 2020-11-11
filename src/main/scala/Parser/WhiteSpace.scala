package Parser

class WhiteSpace extends Parser[Char, Char] {
  override def ask(s: State[Char]): Either[Exception, Char] = {
    s.next() flatMap {c => 
      if(c.isWhitespace) Right(c)
      else Left(new ParserException(s.status, s"expect a whitespace but get $c"))
    }
  }
}

object WhiteSpace {
  def apply(): WhiteSpace = new WhiteSpace()
}
