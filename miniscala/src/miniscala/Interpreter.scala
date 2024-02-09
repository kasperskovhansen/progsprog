package miniscala

import miniscala.Ast.*

import scala.io.StdIn

/**
 * Interpreter for MiniScala.
 */
object Interpreter {

  type VarEnv = Map[Var, Int]

  def eval(e: Exp, venv: VarEnv): Int = {
    e match {
      case IntLit(c) => c
      case VarExp(x) =>
        trace(s"Looking up variable $x")
        venv(x)
      case BinOpExp(leftexp, op, rightexp) =>
        val leftval = eval(leftexp, venv)
        val rightval = eval(rightexp, venv)
        val res = op match {
          case PlusBinOp() =>
            leftval + rightval
          case MinusBinOp() =>
            leftval - rightval
          case MultBinOp() =>
            leftval * rightval
          case DivBinOp() =>
            if (rightval == 0)
              throw InterpreterError(s"Division by zero", op)
            leftval / rightval
          case ModuloBinOp() =>
            if (rightval == 0)
              throw InterpreterError(s"Modulo by zero", op)
            leftval % rightval
          case MaxBinOp() =>
            if (leftval >= rightval) leftval else rightval
        }
        // Doesn't trace the initial expression, only the result and the intermediate steps.
        // This keeps the trace output output more concise.
        trace(s"Evaluating ($leftval ${Unparser.unparse(op)} $rightval) to $res")
        res
      case UnOpExp(op, exp) =>
        val expval = eval(exp, venv)
        op match {
          case NegUnOp() =>
            trace("Evaluating " + op + " " + exp)
            -expval
        }
      case BlockExp(vals, exp) =>
        trace("Opening block")
        var venv1 = venv
        for (d <- vals)
          venv1 = venv1 + (d.x -> eval(d.exp, venv1))
        val result = eval(exp, venv1)
        trace("Closing block")
        result
    }
  }

  /**
   * Builds an initial environment, with a value for each free variable in the program.
   */
  def makeInitialVarEnv(program: Exp): VarEnv = {
    var venv = Map[Var, Int]()
    for (x <- Vars.freeVars(program)) {
      print(s"Please provide an integer value for the variable $x: ")
      venv = venv + (x -> StdIn.readInt())
    }
    venv
  }

  /**
   * Prints message if option -trace is used.
   */
  def trace(msg: String): Unit =
    if (Options.trace)
      println(msg)

  /**
   * Exception thrown in case of MiniScala runtime errors.
   */
  class InterpreterError(msg: String, node: AstNode) extends MiniScalaError(s"Runtime error: $msg", node.pos)
}
