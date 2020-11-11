package Parser

class Return[OUT, IN](val ele: OUT) extends Parser[OUT, IN] {
  override def ask(s: State[IN]): Either[Exception, OUT] = Right(ele)
}

object Return {
  def apply[OUT, IN](ele: OUT): Return[OUT, IN] = new Return(ele)
}
