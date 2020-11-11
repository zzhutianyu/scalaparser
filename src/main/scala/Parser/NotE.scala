package Parser

class NotE[T](val ele: T) extends Parser[T, T] {
  override def ask(s: State[T]): Either[Exception, T] = {
    s.next() flatMap { data =>
      if (ele != data) Right(data)
      else Left(new ParserException(s.status, s"expect a object not $ele at ${s.status} but $data"))
    }
  }
}

object NotE {
  def apply[T](ele: T): NotE[T] = new NotE(ele)
}
