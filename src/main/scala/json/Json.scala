package json

trait Json {
  def show: String
  def mkAst: Json = this
}

