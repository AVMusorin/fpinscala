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
}
