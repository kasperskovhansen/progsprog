package miniscala

import miniscala.Ast._
import miniscala.Interpreter._
import miniscala.TypeChecker.{FunTypeEnv, TypeError, VarTypeEnv, typeCheck}
import miniscala.parser.Parser.parse

object Test49 {

  def main(args: Array[String]): Unit = {
    test("{def f(x: Int): Int = x; f(2)}", IntVal(2), IntType())
    testFail("{def f(x: Int): Int = x; f(2, 3)}")


    // <-- add more test cases here

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

  def testVal(prg: String, value: Val, venv: VarEnv = Map[Var, Val](), fenv: FunEnv = Map[Var, Closure]()): Unit = {
    assert(eval(parse(prg), venv, fenv) == value)
  }

  def testType(prg: String, out: Type, venv: VarTypeEnv = Map[Var, Type](), fenv: FunTypeEnv = Map[Var, (List[Type], Type)]()): Unit = {
    assert(typeCheck(parse(prg), venv, fenv) == out)
  }

  def testValFail(prg: String): Unit = {
    try {
      eval(parse(prg), Map[Var, Val](), Map[Var, Closure]())
      assert(false)
    } catch {
      case _: InterpreterError => assert(true)
    }
  }

  def testTypeFail(prg: String): Unit = {
    try {
      typeCheck(parse(prg), Map[Var, Type](), Map[Var, (List[Type], Type)]())
      assert(false)
    } catch {
      case _: TypeError => assert(true)
    }
  }
}