package miniscala
import miniscala.List.{List => MyList, Cons, Nil, length, append, forall}

object Week7 {

  def main(args: Array[String]): Unit = {
    val list: MyList[Int] = Cons(1, Cons(5, Cons(3, Cons(2, Cons(4, Nil())))))
    println(forall(list, elem => {elem <= 4}))
  }

  def orElse(a: Boolean, b: => Boolean): Boolean = {
    if (a) return true
    if (b) return true
    false
  }

}
