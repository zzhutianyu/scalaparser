package json

case class JObject(val get: Map[JString, Json]) extends Json {
  override def show: String = {
    val res = for ((k, v) <- get) yield s"${k.show}: ${v.show}"
    res.mkString("{", ",", "}")
  }
}
