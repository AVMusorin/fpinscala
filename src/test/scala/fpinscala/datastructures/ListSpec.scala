package fpinscala.datastructures

import org.scalatest.FunSpec
import org.scalatest.Matchers

class ListSpec extends FunSpec with Matchers {
  describe("List data structure") {
    it("should work with ex 3.1") {
      val x = List(1, 2, 3, 4, 5) match {
        case Cons(x, Cons(2, Cons(4, _))) => x
        case Nil => 42
        case Cons(x, Cons(y, Cons(3, Cons(4, _)))) => x + y
        case Cons(h, t) => h + List.sum(t)
        case _ => 101
      }

      x should be (3)
    }

    it("should work with ex 3.2") {
      val l1 = List(1, 2, 3)
      val l2 = List(1)
      val l3: List[Int] = Nil

      List.tail(l1) should be (List(2, 3))
      List.tail(l2) should be (Nil)
      assertThrows[IllegalArgumentException] {
        List.tail(l3)
      }
    }

    it("should work with ex 3.3") {
      val l1 = Nil
      val l2 = List(1, 2)
      val l3 = List(1)

      List.setHead(l1, head = 0) should be (Cons(0, Nil))
      List.setHead(l2, head = 0) should be (Cons(0, Cons(2, Nil)))
      List.setHead(l3, head = 0) should be (Cons(0,Nil))
    }

    it("should work with ex 3.4") {
      val l = Nil
      val l1 = List(1)
      val l2 = List(1, 2)
      val l3 = List(1, 2, 3)

      assertThrows[IllegalArgumentException] {
        List.drop(l, 1)
      }
      assertThrows[IllegalArgumentException] {
        List.drop(l1, -1)
      }

      List.drop(l1, 0) should be (Cons(1, Nil))
      List.drop(l2, 1) should be (Cons(2, Nil))
      List.drop(l3, 2) should be (Cons(3, Nil))
    }

    it("should work with ex 3.5") {
      val l = Nil
      val l1 = List(1, 2, 3, 4, 5)

      List.dropWhile(l, (x: Int) => x > 1) should be(Nil)
      List.dropWhile(l1, (x: Int) => x < 2) should be (List(2, 3, 4, 5))
    }

    it("should work with ex 3.6") {
      val l = List(1, 2, 3, 4)

      List.init(l) should be (List(1, 2, 3))
    }
  }
}
