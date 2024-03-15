package miniscala

import scala.annotation.tailrec

object List {

  sealed abstract class List[T]

  case class Nil[T]() extends List[T]

  case class Cons[T](x: T, xs: List[T]) extends List[T]


  def length[T](xs: List[T]): Int = xs match {
    case Nil() => 0
    case Cons(_, ys) => 1 + length(ys)
  }

  def append[T](xs: List[T], x: T): List[T] = xs match {
    case Nil() => Cons[T](x, Nil[T]())
    case Cons(y, ys) => Cons[T](y, append(ys, x))
  }

  @tailrec
  def foldLeft[A, B](xs: List[A], z: B, f: (B, A) => B): B = xs match {
    case Nil() => z
    case Cons(y, ys) => foldLeft(ys, f(z, y), f)
  }

  def foldRight[A, B](xs: List[A], z: B, f: (A, B) => B): B = xs match {
    case Nil() => z
    case Cons(y, ys) => f(y, foldRight(ys, z, f))
  }

  @tailrec
  def forall[T](xs: List[T], p: (T) => Boolean): Boolean = xs match
    case Nil() => true
    case Cons(y, ys) => if (!p(y)) false else forall[T](ys, p)

  @tailrec
  def exists[T](xs: List[T], p: (T) => Boolean): Boolean = xs match
    case Nil() => false
    case Cons(y, ys) => if(p(y)) true else exists[T](ys, p)

  def count[T](xs: List[T], p: (T) => Boolean): Int = foldLeft[T, Int](xs, 0, (acc, x) => {if (p(x)) acc + 1 else acc})

  def find[T](xs: List[T], p: (T) => Boolean): Option[T] = foldRight[T, Option[T]](xs, None, (x, acc) => {if (p(x)) Some(x) else acc})

}
