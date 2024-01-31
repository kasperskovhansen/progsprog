package miniscala

import miniscala.Ast.*

/**
 * Interpreter for MiniScala.
 */
object Interpreter {

  def eval(e: Exp): Int = {
    e match {
      case IntLit(c) => c
      case BinOpExp(leftexp, op, rightexp) =>
        val leftval = eval(leftexp)
        val rightval = eval(rightexp)
        op match {
          case PlusBinOp() =>
            trace("Adding " + leftval + " and " + rightval)
            leftval + rightval
          case MinusBinOp() =>
            trace("Subtracting " + leftval + " and " + rightval)
            leftval - rightval
          case MultBinOp() =>
            trace("Multiplying " + leftval + " and " + rightval)
            leftval * rightval
          case DivBinOp() =>
            trace("Dividing " + leftval + " and " + rightval)
            if (rightval == 0)
              throw InterpreterError(s"Division by zero", op)
            leftval / rightval
          case ModuloBinOp() =>
            trace(leftval + " modulo " + rightval)
            leftval % rightval
          case MaxBinOp() =>
            trace("Max of " + leftval + " and " + rightval)
            if (leftval >= rightval) leftval else rightval
        }
      case UnOpExp(op, exp) =>
        val expval = eval(exp)
        op match {
          case NegUnOp() =>
            trace("Evaluating " + op + " " + exp)
            -expval
        }
    }
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
