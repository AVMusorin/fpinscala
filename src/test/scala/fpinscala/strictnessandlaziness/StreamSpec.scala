package fpinscala.strictnessandlaziness

import org.scalatest.FunSpec
import org.scalatest.Matchers

class StreamSpec extends FunSpec with Matchers {
  describe("Stream") {
    it("should work with ex. 5.1") {
      val stream = Stream(1, 2, 3)

      stream.toList should be (List(1, 2, 3))
    }

    it("should work with ex. 5.2 take") {
      val stream = Stream(1, 2, 3)
      val stream2 = Empty

      stream.take(2) should be (List(1, 2))
      stream.take(5) should be (List(1, 2, 3))
      stream2.take(2) should be (List.empty)
    }

    it("should work with ex 5.2 drop") {
      val stream = Stream(1, 2, 3)
      stream.drop(1).toList should be (Stream(2, 3).toList)
    }

    it("should work with ex 5.3") {
      val stream = Stream(1, 2, 3)
      stream.takeWhile(x => x <= 2).toList should be (Stream(1, 2).toList)
    }

    it("should work with exists") {
      val stream = Stream(1, 2, 3)
      stream.exists(x => x > 2)
    }

    it("should work with ex 5.4") {
      Stream(1, 2, 3).forAll(x => x < 2) should be (false)
      Stream(1, 2, 3).forAll(x => x < 4) should be (true)
    }

    it("should work with ex 5.5") {
      val stream = Stream(1, 2, 3)
      stream.takeWhileViaFoldRight(x => x <= 2).toList should be (Stream(1, 2).toList)
    }

    it("should work with ex 5.6") {
      Stream(1, 2, 3).headOptionViaFoldRight() should be (Some(1))
      Empty.headOptionViaFoldRight() should be (None)
    }

    it("should work with ex 5.7 map") {
      Stream(1, 2, 3).map(x => x * 2).toList should be (List(2, 4, 6))
      Stream.empty[Int].map(x => x * 3).toList should be (List.empty[Int])
    }

    it("should work with ex 5.7 filter") {
      Stream(1, 2, 3).filter(x => x < 3).toList should be (List(1, 2))
      Stream.empty[Int].filter(x => x < 3).toList should be (List.empty[Int])
    }

    it("should work with ex 5.7 append") {
      Stream(1, 2, 3).append(Stream(4, 5, 6)).toList should be (List(1, 2, 3, 4, 5, 6))
    }

    it("should work with ex 5.7 flatMap") {
      Stream(1, 2, 3).flatMap(x => Stream(x, x)).toList should be (List(1, 1, 2, 2, 3, 3))
    }
  }
}
