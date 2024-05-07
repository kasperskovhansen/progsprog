package miniscala

import miniscala.AbstractMachine._
import miniscala.Compiler._
import miniscala.parser.Parser.parse

object Test132 {

  def main(args: Array[String]): Unit = {
    test("2 + 3", 5)
    test("2 - 3", -1)
    test("2 * 3", 6)
    test("6 / 2", 3)
    test("2 == 3", 0)
    test("2 == 2", 1)
    test("2 < 3", 1)
    test("2 < 1", 0)
    test("2 <= 3", 1)
    test("3 <= 3", 1)
    test("4 <= 3", 0)
    test("false & true", 0)
    test("true & false", 0)
    test("true & true", 1)
    test("false & false", 0)
    test("false | true", 1)
    test("true | false", 1)
    test("true | true", 1)
    test("false | false", 0)
    test("!true", 0)
    test("!false", 1)
    test("-1", -1)
    test("-(-2)", 2)
    test("true", 1)
    test("false", 0)

    test("{ val x = 2; x }", 2)
    test("{ val x = 2; val y = 4; x + y }", 6)
    test("{ val x = 2; val y = x; x + y }", 4)
    // test("{val z = { val x = 2; x }; x }", 2) // Fails as expected
    test("{ 1 }", 1)

    println("All tests passed successfully!")
  }

  def test(prg: String, result: Int): Unit = {
    assert(execute(compile(parse(prg)), Nil) == result)
  }

}