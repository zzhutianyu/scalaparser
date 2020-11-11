package json.parser

import org.specs2.mutable.Specification

class OTest extends Specification {
  "object parser" should {
    "some test " in {
      val p = new JsonParser
      val txt = "{\n  \" s \\u4a4a asdf asdf \": [\"test\\\"     \", \"test\"],\n  \"t\": true,\n  \"te\": null,\n  \"txt\": {\n    \"test\": \"test\"\n  }\n}"
      val res = p ask txt
      res.map {v =>
        println(v.show)
        v
      }
      1 must_==(1)
    }
  }

}
