package fpinscala.handlingerrors

import org.scalatest.FunSpec
import org.scalatest.Matchers
import fpinscala.handlingerrors._

class OptionSpec extends FunSpec with Matchers {
  describe("Option ex. 4.1") {
    it("should work with map") {
      val o = Some(1)
      o.map(x => x * 2) should be (Some(2))
    }

    it("should work with getOtElse") {
      val o = Some(1)
      val o2 = None

      o.getOrElse(3) should be (1)
      o2.getOrElse(5) should be (5)
    }

    it("should work with orElse") {
      val o = Some(1)
      val o2 = None

      o.orElse(Some(2)) should be (Some(1))
      o2.orElse(Some(3)) should be (Some(3))
    }

    it("should work with filter") {
      val o = Some(1)
      val o2: Option[Int] = None

      o.filter(x => x < 2) should be (Some(1))
      o.filter(x => x < 1) should be (None)
      o2.filter(x => x < 2) should be (None)
    }

    it("should work with flatMap") {
      val o = Some(1)
      val o2: Option[Int] = None

      o.flatMap(x => Some(x * 2)) should be(Some(2))
      o2.flatMap(x => Some(x * 2)) should be(None)
    }
  }
}
