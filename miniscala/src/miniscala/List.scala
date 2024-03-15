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
  def forall[T](xs: List[T], p: (T) => Boolean): Boolean = xs match
    case Nil() => true
    case Cons(y, ys) => if (!p(y)) false else forall[T](ys, p)

}
