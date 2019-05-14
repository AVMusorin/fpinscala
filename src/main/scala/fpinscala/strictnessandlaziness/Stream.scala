package fpinscala.strictnessandlaziness

import scala.annotation.tailrec

sealed trait Stream[+A] {

  def headOption: Option[A] = this match {
    case Empty => None
    case Cons(h, _) => Some(h())
  }

  def toList: List[A] = {
    this match {
      case Empty => List.empty[A]
      case Cons(x, xs) => List(x()) ++ xs().toList
    }
  }

  def take(n: Int): List[A] = {
    if (n > 0) {
      this match {
        case Cons(x, xs) => List(x()) ++ xs().take(n - 1)
        case _ => Empty.toList
      }
    } else {
      Empty.toList
    }
  }

  @tailrec
  final def drop(n: Int): Stream[A] = {
    if (n > 0) {
      this match {
        case Cons(_, xs) => xs().drop(n - 1)
        case _ => this
      }
    } else {
      this
    }
  }

  def takeWhile(p: A => Boolean): Stream[A] = {
    this match {
      case Cons(x, xs) if p(x()) => Cons(x, () => xs().takeWhile(p))
      case _ => Empty
    }
  }
  def foldRight[B](z: => B)(f: (A, => B) => B): B = {
    this match {
      case Cons(h, t) => f(h(), t().foldRight(z)(f))
      case _ => z
    }
  }

  @tailrec
  final def foldLeft[B](z: => B)(f: (A, => B) => B): B = {
    this match {
      case Cons(x, xs) => xs().foldLeft(f(x(), z))(f)
      case Empty => z
    }
  }

  def exists(p: A => Boolean): Boolean = foldRight(false)((a, b) => p(a) || b)

  /**
    * stop computation if false
    */
  def forAll(p: A => Boolean): Boolean = foldRight(true)((a, b) => p(a) && b)

  /**
    * stop computation if false
    */
  def takeWhileViaFoldRight(p: A => Boolean): Stream[A] = {
    foldRight(Stream.empty[A]) {
      case (a, b) if p(a) => Cons(() => a , () => b)
      case _ => Stream.empty[A]
    }
  }

  /**
    * hard
    */
  def headOptionViaFoldRight(): Option[A] = this.foldRight(Option.empty[A])((a, _) => Some(a))

  def map[B](f: A => B): Stream[B] = {
    foldRight(Stream.empty[B])((a, b) => Cons(() => f(a), () => b))
  }

  def filter(f: A => Boolean): Stream[A] = {
    foldRight(Stream.empty[A]){
      case (a, b) if f(a) => Cons(() => a, () => b)
      case _ => Empty
    }
  }

  def append[B >: A](b: Stream[B]): Stream[B] = {
    foldRight(b)((a, b) => Cons(() => a, () => b))
  }

  def flatMap[B](f: A => Stream[B]): Stream[B] = {
    foldRight(Stream.empty[B])((a, b) => f(a).append(b))
  }
}

case object Empty extends Stream[Nothing]
case class Cons[+A](h: () => A, t: () => Stream[A]) extends Stream[A]

object Stream {

  def cons[A](hd: => A, tl: => Stream[A]): Stream[A] = {
    lazy val head = hd
    lazy val tail = tl
    Cons(() => head, () => tail)
  }

  def empty[A]: Stream[A] = Empty

  def apply[A](as: A*): Stream[A] =
    if (as.isEmpty) empty else cons(as.head, apply(as.tail: _*))

  /**
    * Создает бесконечный список, где каждый элемент, переданное значение
    */
  def constant[A](a: A): Stream[A] = cons(a, constant(a))

  /**
    * Создает бесконечный список с арифметической прогрессией
    */
  def from(n: Int): Stream[Int] = cons(n, from(n + 1))

  /**
    * Бесконечный список из чисел Фиббоначи
    */
  def fibs(): Stream[Int] = {
    def generateFib(a: Int, b: Int): Stream[Int] = {
      cons(a, generateFib(b, a + b))
    }
    generateFib(0, 1)
  }

  def unfold[A, S](z: S)(f: S => Option[(A, S)]): Stream[A] = {
    f(z) match {
      case Some((x, xs)) => cons(x, unfold(xs)(f))
      case None => empty
    }
  }
}