package Parser

class Hex extends Parser[Char, Char] {
  val hex = ('a' to 'f') ++ ('A' to 'F')
  implicit class charOps(c: Char) {
    def isHex(): Boolean = {
      if(c.isDigit) return true
      if(hex.contains(c)) return true
      false
    }
  }

  val hexP = new Parser[Char, Char] {
    override def ask(s: State[Char]): Either[Exception, Char] = {
      s.next() flatMap {re =>
        if(re.isHex) Right(re)
        else Left(new ParserException(s.status, s"Expect hex char $re"))
      }
    }
  }

  override def ask(s: State[Char]): Either[Exception, Char] = {
    hexP ask s
  }
}
