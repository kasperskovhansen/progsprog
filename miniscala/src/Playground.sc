

class C[+T](a: T) {
  var b: T = a
}

sealed abstract class List[+T] {
  def concat(xs: List[T]): List[T] = this match {
    case Nil => xs
    case Cons(z, zs) => Cons(z, zs.concat(xs))
  }
}

case object Nil extends List[Nothing]

case class Cons[T](x: T, xs: List[T]) extends List[T]

class Person

class Student extends Person

class Teacher extends Person

val a: List[Teacher] = Cons(new Teacher, Cons(new Teacher, Nil))
val b: List[Student] = Cons(new Student, Cons(new Student, Nil))
val c: List[Person] = a.concat(b)