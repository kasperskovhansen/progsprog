package miniscala

import miniscala.Interpreter._
import miniscala.parser.Parser.parse
import scala.collection.immutable.List
object Test112 {

  def main(args: Array[String]): Unit = {
    testValFail("""{ var z = null; z.f }""")
    testVal("""{ class C() { }; { var x: C = null; x = new C() } }""".stripMargin, TupleVal(List[Val]()))
    testVal("""{ class C() { }; { var x: C = new C(); x = null } }""".stripMargin, TupleVal(List[Val]()))
    // <-- add more test cases here
    testVal("""null == null""".stripMargin, BoolVal(true))
    testVal("""{ class C() { }; { var x: C = new C(); x == null } }""".stripMargin, BoolVal(false))
    testValFail("""null.a""".stripMargin)
    testValFail("""null.f()""".stripMargin)

    println("All tests passed successfully!")
  }

  def testVal(prg: String, value: Val, env: Env = Map(), cenv: ClassEnv = Map(), sto: Sto = Map()): Unit = {
    val (res, _) = eval(parse(prg), env, cenv, sto)
    assert(res == value)
  }

  def testValFail(prg: String, env: Env = Map(), cenv: ClassEnv = Map(), sto: Sto = Map()): Unit = {
    try {
      eval(parse(prg), env, cenv, sto)
      assert(false)
    } catch {
      case _: InterpreterError => assert(true)
    }
  }

}