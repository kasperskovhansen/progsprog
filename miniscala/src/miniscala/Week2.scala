package miniscala

import IntList.{Cons, IntList, Nil}
import miniscala.Ast.{BinOpExp, Exp, ValDecl}

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

    // Odd function
    println("Odd")
    println(odd(Cons(2, Cons(5, Cons(3, Nil)))))
    println(odd(Cons(3, Cons(5, Cons(4, Cons(7, Cons(10, Nil)))))))

    // Count IntLits function
    println("CountIntLits")
    println(
      countIntLits(
        Ast.BinOpExp(
          Ast.IntLit(5), Ast.PlusBinOp(),
          Ast.BinOpExp(
            Ast.BlockExp(
              vals = List(
                Ast.ValDecl(
                  "x", Ast.BinOpExp(
                    Ast.IntLit(8), Ast.MinusBinOp(), Ast.IntLit(2)
                  )
                ),
                Ast.ValDecl(
                  "y", Ast.IntLit(13)
                )
              ), exp = Ast.BinOpExp(
                Ast.VarExp("x"), Ast.PlusBinOp(), Ast.IntLit(10)
              )
            ), Ast.MultBinOp(), Ast.IntLit(3)
          )
        )
      )
    )

    val venv: VarEnv = ConsVarEnv("x", 5, ConsVarEnv("y", 7, NilVarEnv))
    val mapEnv: Map[Var, Int] = varEnvToMap(venv)

    println(venv);

    // VarEnv to Map function
    println("VarEnv to Map")
    println(mapEnv.foreach { case (key, value) =>
      println(s"Key: $key, Value: $value")
    })

    // Map to VarEnv function
    println("Map to VarEnv")
    println(mapToVarEnv(mapEnv))

    // Decode Nat function
    println("Decode Nat")
    println(decode(Succ(Succ(Succ(Zero)))))

    // Encode Nat function
    println("Encode 5")
    println(encode(5))

    // Add Nat function
    println("Add Nat function")
    println(add(Succ(Zero), Succ(Succ(Succ(Succ(Succ(Succ(Succ(Zero)))))))))

    // Add Nat function
    println("Mult Nat function")
    println(mult(Succ(Succ(Succ(Zero))), Succ(Succ(Succ(Succ(Zero))))))

  }

  def square(il: IntList): IntList = il match
    case IntList.Nil => Nil
    case Cons(x, xs) => Cons(x * x, square(xs))

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

  def countIntLits(exp: Exp): Int =
    exp match
      case Ast.VarExp(x) => 0
      case Ast.BinOpExp(leftexp, op, rightexp) => countIntLits(leftexp) + countIntLits(rightexp)
      case Ast.UnOpExp(op, exp) => countIntLits(exp)
      case Ast.IntLit(c) => 1
      case Ast.BlockExp(vals, exp) => vals.map(v => countIntLits(v.exp)).sum + countIntLits(exp)

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
    @tailrec
    def conv(env: VarEnv, map: Map[Var, Int]): Map[Var, Int] = env match
      case ConsVarEnv(x, v, next) => conv(next, map + (x -> v))
      case NilVarEnv => map

    conv(env, Map.empty)
  }

  def mapToVarEnv(env: Map[Var, Int]): VarEnv = env.foldLeft(NilVarEnv: VarEnv) {
    case (accEnv, (x, v)) => extend(accEnv, x, v)
  }

  sealed abstract class Nat

  case object Zero extends Nat

  case class Succ(n: Nat) extends Nat

  def decode(nat: Nat): Int = {
    def dec(nat: Nat, acc: Int): Int = nat match
      case Zero => acc
      case Succ(n) => 1 + dec(n, acc)
    dec(nat, 0)
  }

  def encode(value: Int): Nat = {
    @tailrec
    def enc(value: Int, acc: Nat): Nat = {
      if (value == 0) acc else enc(value - 1, Succ(acc))
    }
    enc(value, Zero)
  }

  def add(x: Nat, y: Nat): Nat = x match
    case Zero => y
    case Succ(n) => Succ(add(n, y))

  def mult(x: Nat, y: Nat): Nat = {
    def mul(accx: Nat, accy: Nat): Nat = accx match
      case Zero => accy match
        case Zero => accx
        case Succ(n) => mul(x, n)
      case Succ(n) => Succ(mul(n, accy))
    mul(x, y)
  }

}

