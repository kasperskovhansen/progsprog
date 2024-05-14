package miniscala

import miniscala.AbstractMachine._
import miniscala.Compiler._
import miniscala.parser.Parser.parse

object Test155 {

  def main(args: Array[String]): Unit = {
//    test("{ val x = 2; x }", 2)
//    test("{ val x = 2; val y = 4; x + y }", 6)
//    test("{ val x = 2; val y = x; x + y }", 4)
//    test("{ val x = 2; def g() = { x + 1 }; g() }", 3)
//    test("{ val x = 2; var y = 2; def g() = { y = 1; y }; g() }", 1)

    test("{ def g(n: Int) = n + 2; g(4) }", 6)
    
    // test("{val z = { val x = 2; x }; x }", 2) // Fails as expected
    println("All tests passed successfully!")
  }

  def test(prg: String, result: Int): Unit = {
    assert(execute(compile(parse(prg)), Nil) == result)
  }

}