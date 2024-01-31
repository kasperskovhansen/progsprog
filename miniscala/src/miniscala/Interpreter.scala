package miniscala

import miniscala.Ast.*

/**
  * Interpreter for MiniScala.
  */
object Interpreter {

  def eval(e: Exp): Int = e match {
    case IntLit(c) => c
    case BinOpExp(leftexp, op, rightexp) =>
      val leftval = eval(leftexp)
      val rightval = eval(rightexp)
      op match {
        case PlusBinOp() => leftval + rightval
        case MinusBinOp() => leftval - rightval
        case MultBinOp() => leftval * rightval
        case DivBinOp() =>
          if (rightval == 0)
            throw InterpreterError(s"Division by zero", op)
          leftval / rightval
        case ModuloBinOp() => leftval % rightval
        case MaxBinOp() =>
          if (leftval >= rightval) leftval else rightval
      }
    case UnOpExp(op, exp) =>
      val expval = eval(exp)
      op match {
        case NegUnOp() => -expval
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
