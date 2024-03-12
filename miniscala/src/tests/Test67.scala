package tests

import miniscala.Ast.*
import miniscala.Interpreter.*
import miniscala.TypeChecker.*
import miniscala.Unparser.unparse
import miniscala.parser.Parser.parse

object Test67 {

  def main(args: Array[String]): Unit = {
    test("{def f(x: Int): Int = x; f(2)}", IntVal(2), IntType())
    testFail("{def f(x: Int): Int = x; f(2, 3)}")

    // Functions and scope
    test("{val x: Int = 5; def f(x: Int): Int = x; f(2)}", IntVal(2), IntType())
    test("{val y: Int = 5; def f(x: Int): Int = x + y; f(2)}", IntVal(7), IntType())
    testFail("{def f(x: Int): Int = x + z; f(2)}")
    test("{def f(x: Int, y: Int): Int = x + y; f(2, 3)}", IntVal(5), IntType())
    test("{def f(x: Int, y: Float): Float = x + y; f(2, 3.0f)}", FloatVal(5.0f), FloatType())
    testFail("{def f(x: Int, y: Float): String = x + y; f(2, 3.0f)}")

    test("{ def f(x: Int): Int = x + 1; { def g(x: Int): Int = x + 2; f(2) + g(3) } }", IntVal(8), IntType())
    testFail("{ def f(x: Int): Int = g(x); { def g(x: Int): Int = x + 2; f(2) } }")
    test("{ def f(x: Int): Int = x + 1; { def g(x: Int): Int = f(x); g(2) } }", IntVal(3), IntType())

    test("{ val y = 2; def f(x: Int): Int = x + y; { def g(x: Int): Int = f(x) + y; g(3) } }", IntVal(7), IntType())
    test("{ val x = 2; def f(x: Int): Int = x + 1; { val x = 5; def g(x: Int): Int = x + 2; f(2) + g(3) } }", IntVal(8), IntType())
    test("{ val y = 2; def f(x: Int): Int = x + y; { val y = 5; def g(x: Int): Int = x + y; f(2) + g(3) } }", IntVal(12), IntType())
    test("{ val y = 2; def f(x: Int): Int = x + y; { val y = 5; def g(x: Int): Int = f(x) + y; g(3) } }", IntVal(10), IntType())
    test("{ val y = 2; def f(x: Int): Int = x + y; { val y = y + 2; def g(x: Int): Int = f(x) + y; g(3) } }", IntVal(9), IntType())

    // Recursion
    test("{ def f(x: Int): Int = if (x == 0) 0 else f(x - 1); f(4) }", IntVal(0), IntType())

    // Mutual recursion
    test("{ def isEven(n: Int): Boolean = if (n == 0) true else isOdd(n - 1); def isOdd(n: Int): Boolean = if (n == 0) false else isEven(n - 1); isEven(5)}", BoolVal(false), BoolType())
    test("{ def isEven(n: Int): Boolean = if (n == 0) true else isOdd(n - 1); def isOdd(n: Int): Boolean = if (n == 0) false else isEven(n - 1); isEven(4)}", BoolVal(true), BoolType())

    // Match case
    test("{ def f(x: Int): Int = x; def g(x: Int): Int = x; (f(5), g(2)) match { case (c, d) => c + d } }", IntVal(7), IntType())
    test("{ def f(x: Int): Int = x; def g(x: Int): Int = x; (f(5), g(2)) match { case (c, d) => c + d } }", IntVal(7), IntType())

    // Free variable x in g
    testValFail("{ def f(x: Int): Int = x; def g(y: Int): Int = x; g(1) }")

    // Test lambda functions
    test("{val x: Int = 5; val l = (y: Int) => x + y; l(2)}", IntVal(7), IntType())
    test("{val x: Int = 5; val l = (g: Int => Int) => g(x); l((a: Int) => a * 2)}", IntVal(10), IntType())
    test("{val x: Int = (() => 3)(); val l = (g: Int => Int) => g(x); l((a: Int) => a * 2)}", IntVal(6), IntType())
    test("{val myCurry = (x: Int) => (y: Int) => x + y; myCurry(5)(3)}", IntVal(8), IntType())
    test("{def f(x: Int, y: Int): Int = x + y; def special(g: (Int, Int) => Int): Int => Int => Int = (x: Int) => (y: Int) => g(x,y); special(f)(3)(4)}", IntVal(7), IntType())
    testFail("{val f = (x: Int) => g(x); val g = (x: Int) => x + 2; f(5)}")
    testFail("{val x: Int = 5; val l = (y: String) => x + y; l(2)}")
    test("{val f = (x: Int) => x + 3; def f(x: Int): Int = x + 2; f(1)}", IntVal(3), IntType())
    test("{val f = (x: Int) => x + 3; def f(x: Int): Int = if (x < 10) f(x+1) else x; f(1)}", IntVal(10), IntType())
    test("{val f = (x: Int) => x + 2; {val f = (y: Int) => f(y + 4); def g(z: Int): Int = f(z); g(1)}}", IntVal(7), IntType())
    test("{val x = 5; def f(y: Int): Int = x + y; {val x = 8; f(2)}}", IntVal(7), IntType())
    testFail("{def f(x: Int): Int = x+2; def g(h: Int => Int): Int => Int = (x: Int) => h(x); g(5)}")
    testTypeFail("{def f(x: Int): Int = x+2; def g(h: Int): Int => Int => Int = (x: Int) => h(x); g(5)}")

    println("All tests passed successfully!")
  }

  def test(prg: String, rval: Val, rtype: Type): Unit = {
    testVal(prg, rval)
    testType(prg, rtype)
  }

  def testFail(prg: String): Unit = {
    testValFail(prg)
    testTypeFail(prg)
  }

  def testVal(prg: String, value: Val, env: Env = Map[Id, Val]()): Unit = {
    assert(eval(parse(prg), env) == value)
  }

  def testType(prg: String, out: Type, tenv: TypeEnv = Map[Id, Type]()): Unit = {
    assert(typeCheck(parse(prg), tenv) == out)
  }

  def testValFail(prg: String): Unit = {
    try {
      eval(parse(prg), Map[Id, Val]())
      assert(false)
    } catch {
      case _: InterpreterError => assert(true)
    }
  }

  def testTypeFail(prg: String): Unit = {
    try {
      typeCheck(parse(prg), Map[Id, Type]())
      assert(false)
    } catch {
      case _: TypeError => assert(true)
    }
  }
}