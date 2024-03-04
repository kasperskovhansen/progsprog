package miniscala

import IntList.{Cons, IntList, Nil}
import miniscala.Ast.MiniScalaError

import scala.annotation.tailrec
import scala.util.Random

object Week5 {

  def main(args: Array[String]): Unit = {

    val xs = Cons(1, Cons(4, Cons(5, Nil)))
    val ys = Cons(2, Cons(3, Cons(6, Nil)))

//    println(merge(xs, ys))
//
    val zs = Cons(1, Cons(2, Cons(3, Cons(4, Cons(5, Cons(6, Cons(7, Nil)))))))
//
//    println(split(zs, 5))
//
    val as = Cons(7, Cons(8, Cons(2, Cons(3, Cons(9, Cons(1, Cons(4, Cons(5, Cons(6, Nil)))))))))
//
//    println(mergeSort(as))
//
//    println(randomIntList())
//    println(randomIntList())
//    println(randomIntList())

    testMergeSort()

    val qs = Cons(1, Cons(1, Cons(3, Cons(2, Nil))))
    val ps = Cons(3, Cons(1, Cons(2, Cons(1, Nil))))
    val vs = Cons(7, Cons(7, Cons(2, Cons(3, Cons(9, Cons(1, Cons(4, Cons(7, Cons(6, Nil)))))))))
    val ws = Cons(6, Cons(8, Cons(2, Cons(6, Cons(9, Cons(1, Cons(6, Cons(5, Cons(6, Nil)))))))))
    val is = Cons(7, Cons(7, Cons(2, Cons(3, Cons(9, Cons(1, Cons(4, Cons(7, Cons(6, Nil)))))))))
    val js = Cons(6, Cons(4, Cons(7, Cons(9, Cons(1, Cons(2, Cons(3, Cons(7, Cons(7, Nil)))))))))

    println(permuted(xs, ys))
    println(permuted(qs, ps))
    println(permuted(vs, ws))
    println(permuted(is, js))

  }

  def merge(xs: IntList, ys: IntList): IntList = {

    assert(ordered(xs), "xs is not ordered");
    assert(ordered(ys), "ys is not ordered");

    (xs, ys) match {
      case (Cons(a, as), Cons(b, bs)) =>
        if (a <= b) Cons(a, merge(as, ys))
        else Cons(b, merge(xs, bs))
      case (Cons(_,_), Nil) => xs
      case (Nil, Cons(_,_)) => ys
      case (Nil, Nil) => Nil
    }

  }

  def split(xs: IntList, n: Int): (IntList, IntList) = {

    assert(n >= 0, "n should be positive")

    xs match {
      case Cons(y, ys) =>
        if (n == 0) (Nil, xs)
        else
          val (left, right) = split(ys, n-1)
          (Cons(y, left), right)
      case Nil => throw MiniScalaError("n was larger than the list")
    }

  }

  def randomIntList(): IntList = {

    val rand = new Random()

    def randIL(i: Int): IntList = {
      if (i == 0) Nil
      else
        val randNum = rand.nextInt(100)
        Cons(randNum, randIL(i - 1))
    }

    val length = rand.nextInt(100)
    randIL(length)

  }

  def testMergeSort(): Unit = {

    println("\nTesting MergeSort on 100 random inputs\n")

    for (i <- 0 until 100) {
      val il = randomIntList()
      val sortedList = mergeSort(il)
      val sorted = ordered(sortedList)
      println(s"Input:   $il")
      println(s"Output:  $sortedList")
      println(s"Sorted?  ${if (sorted) "Yes" else "No"}")
      println("")
    }

  }

  def permuted(xs: IntList, ys: IntList): Boolean = {

    if (length(xs) != length(ys)) return false

    @tailrec
    def countElements(xs: IntList, map: Map[Int, Int]): Map[Int, Int] = {
      xs match
        case Nil => map
        case Cons(y, ys) =>
          val oldValue = map.getOrElse(y, 0)
          countElements(ys, map + (y -> (oldValue + 1)))
    }

    val xsMap = countElements(xs, Map.empty[Int, Int])
    val ysMap = countElements(ys, Map.empty[Int, Int])

    xsMap == ysMap

  }

  // From exercises
  def mergeSort(xs: IntList): IntList = {
    val n = length(xs) / 2
    if (n == 0) xs
    else {
      val (left, right) = split(xs, n)
      merge(mergeSort(left), mergeSort(right))
    }
  }


  // From Week2
  def ordered(il: IntList): Boolean = {
    @tailrec
    def ord(il: IntList, prev: Int): Boolean =
      il match
        case IntList.Nil => true
        case Cons(x, xs) => if (x >= prev) ord(xs, x) else false

    ord(il, Int.MinValue)
  }

  def length(il: IntList): Int = {
    il match
      case Nil => 0
      case Cons(y, ys) => 1 + length(ys)
  }

  def concat(xs: IntList, ys: IntList): IntList = {
    xs match
      case Nil => ys
      case Cons(z, zs) => Cons(z, concat(zs, ys))
  }

}
