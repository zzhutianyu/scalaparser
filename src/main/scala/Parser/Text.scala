package Parser

class Text(val text: String, val caseSensitive: Boolean) extends Parser [String, Char] {
  val content = if (caseSensitive) text else text.toLowerCase

  override def ask(s: State[Char]): Either[Exception, String] = {
    var idx = 0
    val sb = new StringBuilder
    for (c <- this.content) {
      s.next() match {
        case Right(data) =>
          val dataChar = if (caseSensitive) data else data.toLower
          if (c != dataChar){
            return Left(new ParserException(s.status, s"Expect $c of $text [$idx] (caseSensitve) at ${s.status} but get $data"))
          }
          idx += 1
          sb += data
        case Left(error) => Left(error)
      }
    }
    Right(sb.toString)
  }
}
