package Parser

class Ahead[OUT, IN](val p: Parser[OUT, IN]) extends Parser[OUT, IN] {
  override def ask(s: State[IN]): Either[Exception, OUT] =  {
    val transaction = s.begin()
    val res = p ask s
    s.rollback(transaction)
    res
  }
}
