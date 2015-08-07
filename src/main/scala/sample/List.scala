package sample

/**
 * Created by wardziniak on 06.08.15.
 */

sealed trait List[+A]
case object Nil extends List[Nothing]
case class Cons[+A](head: A, tail: List[A]) extends List[A]



object List {

  def apply[A](as: A*): List[A] = {
    if (as.isEmpty) Nil
    else Cons(as.head, apply(as.tail: _*))
  }

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

  def append[A](to: List[A], from: List[A]): List[A] = to match {
    case Cons(h, t) => Cons(h, append(t, from))
    case Nil => from
  }

  def appendFR[A](to: List[A], from: List[A]): List[A] = foldRight(to, from)((a,b) => Cons(a, b))

  // Change order of `to` List
  def appendFL[A](to: List[A], from: List[A]): List[A] = foldLeft(to, from)((a, b) => Cons(a, b))

  def reverse[A](list: List[A]): List[A] = {
    def helperFun[A](l: List[A], result: List[A]): List[A] = l match {
      case Nil => result
      case Cons(h, t) => helperFun(t, Cons(h, result))
    }
    helperFun(list, Nil: List[A])
  }

  def reverseFL[A](list: List[A]): List[A] = foldLeft(list, Nil: List[A])(Cons(_,_))

  def flatten[A](lists: List[List[A]]): List[A] = foldRight(lists, Nil: List[A])((a, b) => append(a,b))

  def addOneToList(list: List[Int]): List[Int] = list match {
    case Nil => Nil
    case Cons(h, t) => Cons(h+1, addOneToList(t))
  }

  def addOneToListFR(list: List[Int]): List[Int] = foldRight(list, Nil: List[Int])((h, t) => Cons(h+1, t))

//  def addOneToListFL(list: List[Int]): List[Int] = {
//    def helperFun(list: List[Int], result: List[Int]): List[Int] = list match {
//      case Nil => result
//      case Cons(h, t) => helperFun(t, Cons(h+1, result))
//    }
//    reverse(helperFun(list, Nil: List[Int]))
//  }

  def addOneToListFL(list: List[Int]): List[Int] = foldLeft(foldLeft(list, Nil: List[Int])((h, t) => Cons(h+1, t)), Nil: List[Int])(Cons(_, _))

  def map[A,B](as: List[A])(f: A => B): List[B] = foldRight(as, Nil: List[B])((h, t) => Cons(f(h), t))

  def filter[A](as: List[A])(f: A => Boolean): List[A] = as match {
    case Nil => Nil
    case Cons(h, t) => if (f(h)) Cons(h, filter(t)(f)) else filter(t)(f)
  }

  //def filterFR[A](as: List[A])(f: A => Boolean): List[A] = foldRight()

}
