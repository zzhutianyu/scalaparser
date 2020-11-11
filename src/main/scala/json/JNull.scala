package json

class JNull extends Json {
  override def show: String = "null"
}

object JNull {
  val instance = new JNull

  def apply(): JNull = instance
}
