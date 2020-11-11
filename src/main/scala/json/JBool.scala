package json

case class JBool(val get: Boolean) extends Json {
  override def show: String = get.toString
}
