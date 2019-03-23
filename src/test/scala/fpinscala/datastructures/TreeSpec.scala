package fpinscala.datastructures

import org.scalatest.FunSpec
import org.scalatest.Matchers

class TreeSpec extends FunSpec with Matchers {
  describe("Tree") {
    it("should calculate tree size") {
      val tree: Tree[Int] = Branch(
        Branch(
          Leaf(1), Leaf(2)
        ),
        Branch(
          Leaf(3), Leaf(4))
      )

      Tree.size(tree) should be (7)
    }

    it("should find max value ex 3.26") {
      val tree = Branch(
        Branch(
          Leaf(1), Leaf(2)
        ),
        Branch(
          Leaf(3), Branch(
            Leaf(-1), Leaf(100)
          ))
      )
      Tree.maximum(tree) should be (100)
    }

    it("should find max depth ex 3.27") {
      val tree = Branch(
        Branch(
          Leaf(1), Leaf(2)
        ),
        Branch(
          Leaf(3), Branch(
            Leaf(-1), Branch(Leaf(4), Leaf(8))
          ))
      )
      Tree.depth(tree) should be (4)
    }
  }
}
