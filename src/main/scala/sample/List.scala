package sample

/**
 * Created by wardziniak on 06.08.15.
 */

sealed trait List[+A]
case object Nil extends List[Nothing]
case class Cons[+A](head: A, tail: List[A]) extends List[A]



object List {

  def tail[A](xs: List[A]) = xs match {
    case Nil => Nil
    case Cons(h, t) => t
  }

  def setHead[A](xs: List[A], newHead: A) = xs match {
    case Nil => Nil
    case Cons(h, t) => Cons(newHead, t)
  }

  def drop[A](l: List[A], n: Int): List[A] = {
    if (n <= 0) l
    else l match {
      case Nil => Nil
      case Cons(h, t) => drop(t, n-1)
    }
  }

  def dropWhile[A](l: List[A], f: A => Boolean): List[A] = {
    l match {
      case Nil => Nil
      case Cons(h, t) => if (f(h)) dropWhile(t, f) else l
    }
  }

  def init[A](l: List[A]): List[A] = l match {
    case Cons(h, Nil) => Nil
    case Cons(h, t) => Cons(h, init(t))
    case Nil => Nil
  }

  def foldRight[A,B](xs: List[A], z: B)(f: (A, B) => B): B =
    xs match {
      case Nil => z
      case Cons(h, t) => f(h, foldRight(t, z)(f))
    }

  def length[A](xs: List[A]): Int = foldRight(xs, 0)((a,b) => b + 1)

  def foldLeft[A, B](xs: List[A], z: B)(f: (A, B) => B): B = xs match {
    case Nil => z
    case Cons(h, t) => foldLeft(t, f(h, z))(f)
  }
}
