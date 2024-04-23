package miniscala

import miniscala.Interpreter._
import miniscala.parser.Parser.parse
import  scala.collection.immutable.List

object Test116 {

  def main(args: Array[String]): Unit = {
    testVal("{var x = 0; do {x = x + 1} while (x < 0); x}", IntVal(1))
    testVal("{var x = 0; do {x = x + 1} while (x < 5); x}", IntVal(5))
    testVal("{var x = 0; false && {x = 1; true}; x}", IntVal(0))
    testVal("{var x = 0; true && {x = 1; true}; x}", IntVal(1))
    testVal("{var x = 0; true || {x = 1; false}; x}", IntVal(0))
    testVal("{var x = 0; false || {x = 1; false}; x}", IntVal(1))
    testValFail("true && 5")
    testValFail("false || 5")
    testVal("false && 5", BoolVal(false))
    testVal("true || 5", BoolVal(true))

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