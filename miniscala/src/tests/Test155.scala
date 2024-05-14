package miniscala

import miniscala.AbstractMachine.*
import miniscala.Compiler.*
import miniscala.parser.Parser.parse
import miniscala.Options
import miniscala.Validator.validate
import scala.collection.immutable.List

object Test155 {

  def main(args: Array[String]): Unit = {
    test("{ val x = 2; x }", 2)
    test("{ val x = 2; val y = 4; x + y }", 6)
    test("{ val x = 2; val y = x; x + y }", 4)
    test("{ val x = 2; def g() = { x + 1 }; g() }", 3)
    test("{ val x = 2; var y = 2; def g() = { y = 1; y + 1 }; g() }", 2)
    test("{ def g(n: Int) = n + 2; g(4) }", 6)
    test("{ var x = 0; while(x < 5) { x = x + 1 }; x }", 5)
    test("{def fac(n: Int): Int = {def f(n: Int, acc: Int): Int = if (n == 0) acc else f(n - 1, n * acc); f(n, 1)}; fac(5)}", 120)

    println("nr1")
    validate(List(Alloc, Dup, Const(0), Store, EnterScope, EnterScopeDefs(0), Loop(List(Read(0), Load, Const(5), Lt),List(EnterScopeDefs(0), Read(0), Read(0), Load, Const(1), Add, Store, Const(0), ExitScope(0), Pop)), Const(0), Pop, Read(0), Load, ExitScope(1)))
    println("nr2")
    validate(List(Alloc, Dup, Const(1), Store, EnterScope, EnterScopeDefs(0), Const(1), Branch(List(Const(5)),List(Const(6))), ExitScope(1)))
    println("nr3")
    //validate(List(Pop))
    println("nr4")
    validate(List(Alloc, Dup, Const(1), Store, EnterScope, Alloc, Dup, Alloc, Dup, Const(2), Store, EnterScope, EnterScopeDefs(0), Read(1), EnterScopeDefs(0), Read(0), Const(3), Store, Const(0), Pop, Read(1), Load, ExitScope(0), Read(0), Load, Add, Store, Const(0), Pop, Const(9), ExitScope(1), Store, EnterScope, EnterScopeDefs(0), Read(0), Const(7), Store, Const(0), Pop, Read(1), Load, Read(0), Load, Add, Pop, Read(1), Load, Read(0), Load, Add, ExitScope(2)))

    println("All tests passed successfully!")
  }

  def test(prg: String, result: Int): Unit = {
    assert(execute(compile(parse(prg)), Nil) == result)
  }

}