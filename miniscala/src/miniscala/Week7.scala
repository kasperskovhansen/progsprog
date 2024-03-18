package miniscala

import miniscala.List.{Cons, Nil, append, count, exists, find, foldLeft, forall, length, List as MyList}
import miniscala.Nat.{Nat, Succ, Zero, fold}

import scala.annotation.tailrec

object Week7 {

  def main(args: Array[String]): Unit = {
    val list: MyList[Int] = Cons(1, Cons(5, Cons(3, Cons(2, Cons(4, Nil())))))
    println(forall(list, elem => {
      elem <= 4
    }))
    println(forall(list, elem => {
      elem <= 7
    }))
    println(exists(list, elem => {
      elem == 5
    }))
    println(exists(list, elem => {
      elem == 6
    }))
    println(count(list, elem => {
      elem <= 3
    }))
    println(find(list, elem => {
      elem >= 3
    }))

    val nat: Nat = Succ(Succ(Succ(Succ(Succ(Zero)))))

  }

  def orElse(a: Boolean, b: => Boolean): Boolean = {
    if (a) true
    else b
  }


  type Set[A] = MyList[A]

  def makeEmpty[A](): Set[A] = Nil[A]()

  def isEmpty[A](set: Set[A]): Boolean = length(set) == 0

  def size[A](set: Set[A]): Int = length(foldLeft(set, makeEmpty[A](), (acc: Set[A], x: A) => add(acc, x)))

  def add[A](set: Set[A], x: A): Set[A] = if !contains(set, x) then append(set, x) else set

  def contains[A](set: Set[A], x: A): Boolean = exists(set, (y: A) => x == y)

  def remove[A](set: Set[A], x: A): Set[A] = foldLeft(set, makeEmpty(), (acc: Set[A], y: A) =>
    if x != y then add(acc, y) else acc)

  def union[A](set1: Set[A], set2: Set[A]): Set[A] = foldLeft(set2, set1, (acc: Set[A], y: A) => add(acc, y))

  def intersection[A](set1: Set[A], set2: Set[A]): Set[A] = difference(set1, difference(set1, set2))

  def difference[A](set1: Set[A], set2: Set[A]): Set[A] = foldLeft(set2, set1, (acc: Set[A], y: A) =>
    remove(acc, y))

  // converts a set from our own representation to Scala's
  def toScalaSet[A](set: Set[A]): scala.Predef.Set[A] =
    foldLeft(set, scala.Predef.Set.empty[A], (acc: scala.Predef.Set[A], x: A) => acc + x)

  // converts a Scala list to our representation of a set
  def fromScalaList[A](list: scala.List[A]): Set[A] =
    list.foldLeft(makeEmpty[A]())((acc, x) => add(acc, x))

  //  def map[A, B](set: Set[A], f: A => B): Set[B] = foldLeft(set, Nil(), (acc: Set[B], y: A) => add(acc, f(y)))
}
