package Parser

trait State[+IN] {
  type Status
  type Transaction
  def status: Status

  def begin(): Transaction

  def begin(transaction: Transaction): Transaction

  def commit(transaction: Transaction): Unit

  def rollback(transaction: Transaction): Unit

  def next(): Either[Exception, IN]
}

object State {
  def apply(str: String): TxtState = new TxtState(str)

  def apply[T](c: Seq[T]): CommonState[T] = new CommonState[T] {
    override val content: Seq[T] = c;
  }
}
