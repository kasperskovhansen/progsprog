package miniscala
import miniscala.List.{Cons, Nil, append, count, exists, find, forall, length, List as MyList}
import miniscala.Nat.{Nat, Succ, Zero, fold}

import scala.annotation.tailrec

object Week7 {

  def main(args: Array[String]): Unit = {
    val list: MyList[Int] = Cons(1, Cons(5, Cons(3, Cons(2, Cons(4, Nil())))))
    println(forall(list, elem => {elem <= 4}))
    println(forall(list, elem => {elem <= 7}))
    println(exists(list, elem => {elem == 5}))
    println(exists(list, elem => {elem == 6}))
    println(count(list, elem => {elem <= 3}))
    println(find(list, elem => {elem >= 3}))
    
    val nat: Nat = Succ(Succ(Succ(Succ(Succ(Zero)))))
    
  }

  def orElse(a: Boolean, b: => Boolean): Boolean = {
    if (a) true
    else b
  }

//  @tailrec
//  def foldLeft[A, B](xs: List[A], z: B, f: (B, A) => B): B = xs match {
//    case Nil() => z
//    case Cons(y, ys) => foldLeft(ys, f(z, y), f)
//  }

//  def zipOrElse[A,B](xs: MyList[A], ys: MyList[B], f: => List[(A,B)]): List[(A, B)] = foldLeft[(A, B), List[(A,B)]]((xs,ys), (Nil, Nil), (a, b) => {})

}
