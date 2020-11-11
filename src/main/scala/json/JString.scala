package json

case class JString(val get: String) extends Json {
  override def show: String = "\"" + get + "\""
}
