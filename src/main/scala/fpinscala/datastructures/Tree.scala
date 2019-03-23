package fpinscala.datastructures

import scala.annotation.tailrec

sealed trait Tree[+A]
case class Leaf[A](value: A) extends Tree[A]
case class Branch[A](left: Tree[A], right: Tree[A]) extends Tree[A]

object Tree {
  def size[A](t: Tree[A]): Int = {
    // use list for tailrec
    @tailrec
    def size2(acc: Int, trees: List[Tree[A]]): Int = {
      trees match {
        case Nil => acc
        case Cons(x, xs) =>
          x match {
            case Leaf(_) => size2(acc + 1, xs)
            case Branch(l, r) => size2(acc + 1, Cons(l, Cons(r, xs)))
          }
      }
    }
    size2(0, Cons(t, Nil))
  }

  def maximum(t: Tree[Int]): Int = {
    t match {
      case Leaf(v) => v
      case Branch(l, r) => maximum(l) max maximum(r)
    }
  }

  def depth[A](t: Tree[A]): Int = {
    def depthR(t: Tree[A], d: Int): Int = {
      t match {
        case Leaf(_) => d
        case Branch(l, r) => depthR(l, d + 1) max depthR(r, d + 1)
      }
    }
    depthR(t, 0)
  }

  def map[A, B](t: Tree[A])(f: A => B): Tree[B] = {
    t match {
      case Leaf(v) => Leaf(f(v))
      case Branch(l, r) => Branch(map(l)(f), map(r)(f))
    }
  }
}
