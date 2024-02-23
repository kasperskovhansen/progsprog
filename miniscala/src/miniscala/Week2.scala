package miniscala

import IntList.{Cons, IntList, Nil}
import miniscala.Ast.{BinOpExp, Exp, MinusBinOp, ValDecl}
import miniscala.Unparser.unparse
import miniscala.Simplifier.simplify
import miniscala.parser.Parser

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
//    println(
//      countIntLits(
//        Ast.BinOpExp(
//          Ast.IntLit(5), Ast.PlusBinOp(),
//          Ast.BinOpExp(
//            Ast.BlockExp(
//              vals = List(
//                Ast.ValDecl(
//                  "x", Option(Ast.IntType()), Ast.BinOpExp(
//                    Ast.IntLit(8), Ast.MinusBinOp(), Ast.IntLit(2)
//                  )
//                ),
//                Ast.ValDecl(
//                  "y", Option(Ast.IntType()), Ast.IntLit(13)
//                )
//              ), exp = Ast.BinOpExp(
//                Ast.VarExp("x"), Ast.PlusBinOp(), Ast.IntLit(10)
//              )
//            ), Ast.MultBinOp(), Ast.IntLit(3)
//          )
//        )
//      )
//    )

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
    println(decode(mult(encode(2), encode(3))))

    // Add Nat function
    println("Power Nat function")
    println(decode(power(encode(2), encode(3))))

    // Decrement Nat function
    println("Decrement Nat function")
    println(decode(decrement(encode(15))))
    println(decode(decrement(encode(0))))

    // HANDIN
//    println(
//      unparse(
//        Ast.BinOpExp(
//          Ast.IntLit(5), Ast.PlusBinOp(),
//          Ast.BinOpExp(
//            Ast.BlockExp(
//              vals = List(
//                Ast.ValDecl(
//                  "x", Option(Ast.IntType()), Ast.BinOpExp(
//                    Ast.IntLit(8), Ast.MinusBinOp(), Ast.IntLit(2)
//                  )
//                ),
//                Ast.ValDecl(
//                  "y", Option(Ast.IntType()), Ast.IntLit(13)
//                )
//              ), exp = Ast.BinOpExp(
//                Ast.VarExp("x"), Ast.PlusBinOp(), Ast.IntLit(10)
//              )
//            ), Ast.MultBinOp(), Ast.IntLit(3)
//          )
//        )
//      )
//    )

    println(simplify(Parser.parse("5-5")))
    println(simplify(Parser.parse("5-3")))

    // Her koerer vi en masse tests med Simplifier.test()
    // Foerste parameter: Input
    // Anden parameter: Expected output
    // I konsollen bliver resultatet printet
    Simplifier.test("5+0", "5")
    Simplifier.test("3+4", "(3+4)")
    Simplifier.test("0+4", "4")
    Simplifier.test("0+0", "0")
    Simplifier.test("5-5", "0")
    Simplifier.test("4-5", "(4-5)")
    Simplifier.test("1*5", "5")
    Simplifier.test("5*3", "(5*3)")
    Simplifier.test("1*3", "3")
    Simplifier.test("1*0", "0")
    Simplifier.test("0*1", "0")
    Simplifier.test("0*0", "0")
    Simplifier.test("5/0", "(5/0)")
    Simplifier.test("5/1", "5")
    Simplifier.test("0/5", "0")
    Simplifier.test("0%5", "0")
    Simplifier.test("5%1", "0")
    Simplifier.test("5%3", "(5%3)")
    Simplifier.test("5max4", "(5 max 4)")
    Simplifier.test("5 max 4", "(5 max 4)")
    Simplifier.test("4max5", "(4 max 5)")
    Simplifier.test("5max5", "5")
    Simplifier.test("-5", "(-5)")
    Simplifier.test("-5", "(-5)")
    Simplifier.test("-(-(-(1 + 0)))", "(-1)")
    Simplifier.test("(1+0)*(0+1)", "1")
    Simplifier.test("{val x = 1*0; val y = -(-(1)); 1 * x + y}", "{val x = 0;val y = 1;(x+y)}")


    println("VarEnv:")
    // Test VarEnv
    var env = makeEmpty()
    env = env.extend("x", 5)
    env = env.extend("y", 7)
    println(env.lookup("x")) // 5
    //    var env2 = makeEmpty()
    //    println(env2.lookup("x")) // 5

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
//      case Ast.BlockExp(vals, exp) => vals.map(v => countIntLits(v.exp)).sum + countIntLits(exp)

  type Var = String

  sealed abstract class VarEnv {
    def extend(x: Var, v: Int): VarEnv = ConsVarEnv(x, v, this)

    def lookup(x: Var): Int
  }

  private case class ConsVarEnv(x: Var, v: Int, next: VarEnv) extends VarEnv {
    def lookup(x: Var): Int = {
      if (this.x.equals(x)) v else next.lookup(x)
    }
  }

  private case object NilVarEnv extends VarEnv {
    def lookup(x: Var): Int = throw new RuntimeException(s"Variable ${x} not found")
  }

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

  def mult(x: Nat, y: Nat): Nat = x match
    case Zero => Zero
    case Succ(n) => add(mult(n, y), y)

  def power(x: Nat, y: Nat): Nat = y match
    case Zero => Succ(Zero)
    case Succ(n) => mult(x, power(x, n))

  def decrement(x: Nat): Nat = x match
    case Zero => Zero
    case Succ(n) => n

}

