package fpinscala.state

import org.scalatest.FunSpec
import org.scalatest.Matchers

class StateSpec extends FunSpec with Matchers {
  describe("State") {
    it("should generate random non negative numbers in pure form ex 6.1") {
      val random = SimpleRNG(1L)
      val (n, r) = RNG.nonNegativeInt(random)
      n should be (384748)
      r should be (SimpleRNG(25214903928L))
    }

    it("should generate random double numbers between 0 and 1 ex 6.2") {
      val random = SimpleRNG(1L)
      val (n, r) = RNG.double(random)
      val (n1, r1) = RNG.double(r)
      val (n2, _) = RNG.double(r1)

      for (i <- Seq(n, n1, n2)) {
        assert(i >= 0 && i < 1)
      }
    }

    it("should generate ints random list ex. 6.4") {
      val random = SimpleRNG(1L)
      val (l, r) = RNG.ints(3)(random)
      l should be (List(-549383847, -1151252339, 384748))
      r should be (SimpleRNG(245470556921330L))
    }
  }
}
