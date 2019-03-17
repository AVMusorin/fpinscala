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

    it("should work with foldRight") {
      val l = List(1, 2, 3)

      List.foldRight(l, 1)(_ * _) should be (6)
    }

    it("should work with ex 3.9") {
      val l = List(1, 2, 3, 4)
      val l1 = Nil

      List.length(l) should be (4)
      List.length(l1) should be (0)
    }

    it("should work with foldLeft ex 3.10") {
      val l = List(1, 2, 3)

      List.foldLeft(l, 0)(_ + _) should be (6)
    }

    it("should work with ex 3.12") {
      val l = List(1, 2, 3)
      List.reverse(l) should be (List(3, 2, 1))
    }

    it("should work with ex 3.13") {
      val l = List(1, 2, 3)
      List.foldRightViaFoldLeft(l, 0)(_ + _) should be(6)
    }

    it("should work with ex 3.14") {
      val l = List(1, 2, 3)
      List.append(l, List(5)) should be(List(1, 2, 3, 5))
    }

    it("should work with ex 3.18") {
      val l = List(1, 2, 3)
      List.map(l)(x => x * 2) should be(List(2, 4, 6))
    }

    it("should work with ex 3.19") {
      val l = List(1, 5, 10, 1)
      List.filter(l)(x => x % 2 == 0) should be(List(10))
    }

    it("should work with ex 3.20") {
      val l = List(1, 2, 3)
      List.flatMap(l)(x => List(x, x)) should be(List(1, 1, 2, 2, 3, 3))
    }

    it("should work with ex 3.21") {
      val l = List(1, 5, 10, 1)
      List.filter(l)(x => x % 2 == 0) should be(List(10))
    }

    it("should work with ex 3.23") {
      val l1, l2 = List(1, 2, 3)
      val l3 = List(1, 2)
      List.zipWith(l1, l2)((a, b) => a + b) should be (List(2, 4, 6))
      List.zipWith(l1, Nil)((a, b) => a + b) should be (Nil)
      List.zipWith(l1, l3)((a, b) => a + b) should be (List(2, 4))
    }
  }
}
