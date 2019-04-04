package fpinscala.handlingerrors

trait Option[+A] {
  def map[B](f: A => B): Option[B] = {
    this match {
      case None => None
      case Some(a) => Some(f(a))
    }
  }

  def flatMap[B](f: A => Option[B]): Option[B] = {
    this.map(f).getOrElse(None)
  }

  def getOrElse[B >: A](default: => B): B = {
    this match {
      case None => default
      case Some(o) => o
    }
  }

  def orElse[B >: A](ob: => Option[B]): Option[B] = {
    this match {
      case None => ob
      case Some(_) => this
    }
  }

  def filter(f: A => Boolean): Option[A] = {
    this match {
      case Some(a) if f(a) => this
      case _ => None
    }
  }
}

case class Some[+A](v: A) extends Option[A]
case object None extends Option[Nothing]

object Option {
  def lift[A, B](f: A => B): Option[A] => Option[B] = _ map f

  def map2[A, B, C](a: Option[A], b: Option[B])(f: (A, B) => C): Option[C] = {
    (a, b) match {
      case (Some(vA), Some(vB)) => Some(f(vA, vB))
      case _ => None
    }
  }

  def sequence[A](a: List[Option[A]]): Option[List[A]] = {
    a match {
      case Nil => Some(Nil)
      case x :: xs => x flatMap(x => sequence(xs).map(x :: _))
    }
  }

  def traverse[A, B](a: List[A])(f: A => Option[B]): Option[List[B]] = {
    a match {
      case Nil => Some(Nil)
      case x :: xs => map2(f(x), traverse(xs)(f))(_ :: _)
    }
  }
}
