package json

case class JArray(val get: Seq[Json]) extends Json {
  override def show: String = {
    val res = for(value <- get) yield value.show
    res.mkString("[", ",", "]")
  }
}
