package Parser

import java.io.EOFException

trait CommonState[T] extends State[T] {
  override type Status = Int
  override type Transaction = Int

  val content: Seq[T]
  var current: Int = 0
  val NOT_TRANSACTION: Int = -1
  var transaction: Int = NOT_TRANSACTION

  @throws[EOFException]
  def next(): Either[Exception, T] = {
    if (content.size <= current){
      Left(new EOFException())
    } else {
      val re = content(current)
      current += 1
      Right(re)
    }
  }

  override def status: Status = current

  override def begin(): Int = {
    if (transaction == NOT_TRANSACTION) transaction = current
    current
  }

  override def begin(transaction: Int): Int = {
    if (transaction != NOT_TRANSACTION) this.transaction = transaction
    this.transaction
  }

  override def commit(transaction: Int): Unit = if (this.transaction == transaction) this.transaction = NOT_TRANSACTION

  override def rollback(transaction: Int): Unit = {
    if (this.transaction == transaction) this.transaction = NOT_TRANSACTION
    this.current = transaction
  }

}
