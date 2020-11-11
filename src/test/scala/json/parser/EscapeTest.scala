package json.parser

import Parser.TxtState
import org.specs2.mutable.Specification

class EscapeTest extends Specification {
  "sample" should {
    "some test " in {
      val t = TxtState("\\\"")
      val eP = new Escape
      val res = eP ask t
      res.getOrElse("test") must_==("\"")
      val hex = TxtState("\\u34a3")
      val resHex = eP ask hex
      resHex must_==(Right("u34a3"))

    }
  }
}
