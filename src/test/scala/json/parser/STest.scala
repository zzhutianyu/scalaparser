package json.parser

import json.JString
import org.specs2.mutable.Specification

class STest extends Specification {
  "JString parser" should {
    "some test" in {
      val text = "\"asdfasdbdeldfl;jasd;g"
      val p = new S
      val res = p ask text

      res should not be(JString("asdf"))
    }
  }

}
