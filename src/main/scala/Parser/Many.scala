package Parser


import scala.annotation.tailrec

class Many[OUT, IN](val parser: Parser[OUT, IN]) extends Parser[Seq[OUT], IN] {
  val psr = new Attempt[OUT, IN](parser)

  override def ask(s: State[IN]): Either[Exception, Seq[OUT]] = {
    @tailrec
    @inline
    def loop(next: List[OUT]): Seq[OUT] = {
      psr ask s match {
        case Right(result) => loop(next :+ result)
        case Left(_) => next
      }
    }
    Right(loop(List()))
  }
}
