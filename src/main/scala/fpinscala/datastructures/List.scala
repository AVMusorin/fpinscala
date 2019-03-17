package fpinscala.datastructures

import scala.annotation.tailrec

sealed trait List[+A]
case object Nil extends List[Nothing]
case class Cons[+A](head: A, tail: List[A]) extends List[A]

object List {
  def sum(ints: List[Int]): Int = {
    ints match {
      case Nil => 0
      case Cons(x, xs) => x + sum(xs)
    }
  }

  def apply[A](as: A*): List[A] = {
    if (as.isEmpty) {
      Nil
    } else {
      Cons(as.head, apply(as.tail: _*))
    }
  }

  def tail[A](list: List[A]): List[A] = {
    list match {
      case Nil => throw new IllegalArgumentException("list is empty")
      case Cons(_, xs) => xs
    }
  }

  def head[A](list: List[A]): A = {
    list match {
      case Nil => throw new IllegalArgumentException("list is empty")
      case Cons(x, _) => x
    }
  }

  def setHead[A](list: List[A], head: A): List[A] = {
    list match {
      case Nil => List(head)
      case Cons(_, xs) => Cons(head, xs)
    }
  }

  /** removes the first n elements from a list **/
  def drop[A](l: List[A], n: Int): List[A] = {
    require(n >= 0, "n should be more or equal 0")
    @tailrec
    def getTail(l: List[A], n: Int): List[A] = {
      if (n > 0) {
        getTail(tail(l), n - 1)
      } else {
        l
      }
    }
    l match {
      case Nil => throw new IllegalArgumentException("list is empty")
      case Cons(_, _) => getTail(l, n)
    }
  }

  /** removes elements from the List prefix as long as they match a predicate */
  @tailrec
  def dropWhile[A](l: List[A], f: A => Boolean): List[A] = {
    l match {
      case Cons(x, xs) if f(x) => dropWhile(xs, f)
      case _ => l
    }
  }

  def init[A](l: List[A]): List[A] = {
    l match {
      case Nil => throw new IllegalArgumentException("empty list")
      case Cons(x, Cons(_, Nil)) => Cons(x, Nil)
      case Cons(x, xs) => Cons(x, init(xs))
    }
  }

  def foldRight[A, B](as: List[A], z: B)(f: (A, B) => B): B = {
    as match {
      case Nil => z
      case Cons(x, xs) => f(x, foldRight(xs, z)(f))
    }
  }

  @tailrec
  def foldLeft[A, B](as: List[A], z: B)(f: (B, A) => B): B = {
    as match {
      case Nil => z
      case Cons(x, xs) => foldLeft(xs, f(z, x))(f)
    }
  }

  def length[A](as: List[A]): Int = {
    foldRight(as, 0)((_, acc) => acc + 1)
  }

  def reverse[A](as: List[A]): List[A] = {
    foldLeft(as, Nil: List[A])((xs, x) => Cons(x, xs))
  }

  def foldRightViaFoldLeft[A, B](as: List[A], z: B)(f: (A, B) => B): B = {
    foldLeft(reverse(as), z)((a, b) => f(b, a))
  }

  def append[A](as: List[A], b: List[A]): List[A] = {
    foldLeft(reverse(as), b)((xs, x) => Cons(x, xs))
  }

  def map[A, B](l: List[A])(f: A => B): List[B] = {
    foldRight(l, Nil: List[B])((x, xs) => Cons(f(x), xs))
  }

  def filter[A](as: List[A])(f: A => Boolean): List[A] = {
    foldRight(as, Nil: List[A])((x, xs) => if (f(x)) Cons(x, xs) else xs)
  }

  def flatMap[A, B](as: List[A])(f: A => List[B]): List[B] = {
    foldRight(map(as)(f), Nil: List[B])(append)
  }

  def filterViaFlatMap[A](l: List[A])(f: A => Boolean): List[A] = {
    flatMap(l)(x => if (f(x)) List(x) else Nil)
  }

  def zipWith[A, B, C](l1: List[A], l2: List[B])(f: (A, B) => C): List[C] = {
    (l1, l2) match {
      case (Nil, _) => Nil
      case (_, Nil) => Nil
      case (Cons(x1, xs1), Cons(x2, xs2)) => Cons(f(x1, x2), zipWith(xs1, xs2)(f))
    }
  }
}
