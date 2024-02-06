package miniscala
import IntList.{Cons, IntList, Nil}
import scala.annotation.tailrec

object Week2 {

  def main(args: Array[String]): Unit = {

    // Square function
    println("Square")
    println(square(Cons(2, Cons(5, Cons(3, Nil)))))

    // Ordered function
    println("Ordered")
    println(ordered(Cons(-15, Cons(-1, Cons(5, Cons(10, Nil))))))
    println(ordered(Cons(-15, Cons(-15, Cons(5, Cons(10, Nil))))))
    println(ordered(Cons(-15, Cons(10, Cons(5, Cons(10, Nil))))))

    // Odd
    println("Odd")
    println(odd(Cons(2, Cons(5, Cons(3, Nil)))))
    println(odd(Cons(3, Cons(5, Cons(4, Cons(7, Cons(10, Nil)))))))

  }

  def square(il: IntList): IntList = il match
    case IntList.Nil => Nil
    case Cons(x, xs) => Cons(x*x, square(xs))

  def ordered(il: IntList): Boolean = {
    @tailrec
    def ord(il: IntList, prev: Int): Boolean =
      il match
        case IntList.Nil => true
        case Cons(x, xs) => if (x >= prev) ord(xs, x) else false
    ord(il, Int.MinValue)
  }

  def odd(il: IntList): IntList = {
    def odd_item(il: IntList, isOdd: Boolean): IntList =
      il match
        case IntList.Nil => IntList.Nil
        case Cons(x, xs) => if (isOdd) Cons(x, odd_item(xs, !isOdd)) else odd_item(xs, !isOdd)
    odd_item(il, true)
  }

  type Var = String

  sealed abstract class VarEnv

  private case class ConsVarEnv(x: Var, v: Int, next: VarEnv) extends VarEnv

  private case object NilVarEnv extends VarEnv

  def makeEmpty(): VarEnv = NilVarEnv

  def extend(e: VarEnv, x: Var, v: Int): VarEnv = ConsVarEnv(x, v, e)

  def lookup(e: VarEnv, x: Var): Int = e match {
    case ConsVarEnv(y, w, next) => if (x == y) w else lookup(next, x)
    case NilVarEnv => throw new RuntimeException("not found")
  }

  def varEnvToMap(env: VarEnv): Map[Var, Int] = {
    def conv(env: VarEnv, map: Map[Var, Int]): Map[Var, Int] = env match
      case ConsVarEnv(x, v, next) => Map.empty // HER SKAL DER LAVES VIDERE!
      case NilVarEnv => Map.empty
    conv(env, Map.empty)
  }

  def mapToVarEnv(env: Map[Var, Int]): VarEnv = ???
}
