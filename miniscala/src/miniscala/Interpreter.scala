package miniscala

import miniscala.Ast.*
import miniscala.Unparser.unparse

import scala.io.StdIn

/**
 * Interpreter for MiniScala.
 */
object Interpreter {

  sealed abstract class Val

  case class IntVal(v: Int) extends Val

  case class BoolVal(v: Boolean) extends Val

  case class FloatVal(v: Float) extends Val

  case class StringVal(v: String) extends Val

  case class TupleVal(vs: List[Val]) extends Val

  type VarEnv = Map[Var, Val]

  def eval(e: Exp, venv: VarEnv): Val = e match {
    case IntLit(c) => IntVal(c)
    case BoolLit(c) => ???
    case FloatLit(c) => ???
    case StringLit(c) => ???
    case VarExp(x) =>
      venv.getOrElse(x, throw InterpreterError(s"Unknown identifier '$x'", e))
    case BinOpExp(leftexp, op, rightexp) =>
      val leftval = eval(leftexp, venv)
      val rightval = eval(rightexp, venv)
      val res = op match {
        case PlusBinOp() =>
          (leftval, rightval) match {
            case (IntVal(v1), IntVal(v2)) => IntVal(v1 + v2)
            case (FloatVal(v1), FloatVal(v2)) => FloatVal(v1 + v2)
            case (IntVal(v1), FloatVal(v2)) => FloatVal(v1 + v2)
            case (FloatVal(v1), IntVal(v2)) => FloatVal(v1 + v2)
            case (StringVal(v1), StringVal(v2)) => StringVal(v1 + v2)
            case (StringVal(v1), IntVal(v2)) => StringVal(v1 + v2.toString)
            case (StringVal(v1), FloatVal(v2)) => StringVal(v1 + v2.toString)
            case (IntVal(v1), StringVal(v2)) => StringVal(v1.toString + v2)
            case (FloatVal(v1), StringVal(v2)) => StringVal(v1.toString + v2)
            case _ => throw InterpreterError(s"Type mismatch at '+', unexpected values ${valueToString(leftval)} and ${valueToString(rightval)}", op)
          }
        case MinusBinOp() => ???
        case MultBinOp() => ???
        case DivBinOp() =>
          if (rightval == IntVal(0) || rightval == FloatVal(0.0f))
            throw InterpreterError(s"Division by zero", op)
          (leftval, rightval) match {
            case (IntVal(v1), IntVal(v2)) => IntVal(v1 / v2)
            case (FloatVal(v1), FloatVal(v2)) => FloatVal(v1 / v2)
            case (IntVal(v1), FloatVal(v2)) => FloatVal(v1 / v2)
            case (FloatVal(v1), IntVal(v2)) => FloatVal(v1 / v2)
            case _ => throw InterpreterError(s"Type mismatch at '/', unexpected values ${valueToString(leftval)} and ${valueToString(rightval)}", op)
          }
        case ModuloBinOp() => ???
        case EqualBinOp() => ???
        case LessThanBinOp() => ???
        case LessThanOrEqualBinOp() => ???
        case MaxBinOp() => ???
        case AndBinOp() => ???
        case OrBinOp() => ???
      }
      // Doesn't trace the initial expression, only the result and the intermediate steps.
      // This keeps the trace output output more concise.
      trace(s"Evaluating ($leftval ${Unparser.unparse(op)} $rightval) to $res")
      res
    case UnOpExp(op, exp) =>
      val expval = eval(exp, venv)
      op match {
        case NegUnOp() =>
          expval match {
            case IntVal(v) => IntVal(-v)
            case FloatVal(v) => FloatVal(-v)
            case _ => throw InterpreterError(s"Type mismatch at '-', unexpected value ${valueToString(expval)}", op)
          }
        case NotUnOp() => ???
      }
    case IfThenElseExp(condexp, thenexp, elseexp) => ???
    case BlockExp(vals, exp) =>
      trace("Opening block")
      var venv1 = venv
      for (d <- vals)
        val newVal = eval(d.exp, venv1)
        trace(s"Binding $newVal to $d.x")
        venv1 = venv1 + (d.x -> newVal)
      val result = eval(exp, venv1)
      trace("Closing block")
      trace(s"Block evaluated to $result")
      result
    case TupleExp(exps) =>
      var vals = List[Val]()
      for (exp <- exps)
        vals = eval(exp, venv) :: vals
      TupleVal(vals.reverse)
    case MatchExp(exp, cases) =>
      val expval = eval(exp, venv)
      expval match {
        case TupleVal(vs) =>
          for (c <- cases) {
            if (vs.length == c.pattern.length) {
              ???
            }
          }
          throw InterpreterError(s"No case matches value ${valueToString(expval)}", e)
        case _ => throw InterpreterError(s"Tuple expected at match, found ${valueToString(expval)}", e)
      }
  }

  /**
   * Checks whether value `v` has type `ot` (if present), generates runtime type error otherwise.
   */
  def checkValueType(v: Val, ot: Option[Type], n: AstNode): Unit = ot match {
    case Some(t) =>
      (v, t) match {
        case (IntVal(_), IntType()) |
             (BoolVal(_), BoolType()) |
             (FloatVal(_), FloatType()) |
             (IntVal(_), FloatType()) |
             (StringVal(_), StringType()) => // do nothing
        case (TupleVal(vs), TupleType(ts)) if vs.length == ts.length =>
          for ((vi, ti) <- vs.zip(ts))
            checkValueType(vi, Some(ti), n)
        case _ =>
          throw InterpreterError(s"Type mismatch: value ${valueToString(v)} does not match type ${unparse(t)}", n)
      }
    case None => // do nothing
  }

  /**
   * Converts a value to its string representation (for error messages).
   */
  def valueToString(v: Val): String = v match {
    case IntVal(c) => c.toString
    case FloatVal(c) => c.toString
    case BoolVal(c) => c.toString
    case StringVal(c) => c
    case TupleVal(vs) => vs.map(valueToString).mkString("(", ",", ")")
  }

  /**
   * Builds an initial environment, with a value for each free variable in the program.
   */
  def makeInitialVarEnv(program: Exp): VarEnv = {
    var venv = Map[Var, Val]()
    for (x <- Vars.freeVars(program)) {
      print(s"Please provide an integer value for the variable $x: ")
      venv = venv + (x -> IntVal(StdIn.readInt()))
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
