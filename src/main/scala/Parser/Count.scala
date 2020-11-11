package Parser
import scala.annotation.tailrec
import scala.collection.immutable.Seq

class Count[OUT, IN](val p: Parser[OUT, IN], val n: Int) extends Parser[Seq[OUT], IN]{

  override def ask(s: State[IN]): Either[Exception, Seq[OUT]] = {
    @tailrec
    @inline
    def loop(n: Int, next: List[OUT]): Either[Exception, Seq[OUT]] = n match {
      case 0 => Right(next)
      case _ => {
        p ask s match {
          case Right(value) => loop(n - 1, next :+ value )
          case Left(error) => Left(error)
        }
      }
    }
    loop(n, List())
  }
}

object Count {
  def apply[OUT, IN](p: Parser[OUT, IN], n: Int): Count[OUT, IN] = new Count(p, n)
}
