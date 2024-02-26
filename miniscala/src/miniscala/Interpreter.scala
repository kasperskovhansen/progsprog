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

  case class Closure(params: List[FunParam], optrestype: Option[Type], body: Exp, venv: VarEnv, fenv: FunEnv, defs: List[DefDecl])

  type VarEnv = Map[Var, Val]

  type FunEnv = Map[Fun, Closure]

  /**
   * Evaluates an expression.
   */
  def eval(e: Exp, venv: VarEnv, fenv: FunEnv): Val = e match {
    case IntLit(c) => IntVal(c)
    case BoolLit(c) => BoolVal(c)
    case FloatLit(c) => FloatVal(c)
    case StringLit(c) => StringVal(c)
    case VarExp(x) =>
      venv.getOrElse(x, throw InterpreterError(s"Unknown identifier '$x'", e))
    case BinOpExp(leftexp, op, rightexp) =>
      val leftval = eval(leftexp, venv, fenv)
      val rightval = eval(rightexp, venv, fenv)
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
            case _ => throw InterpreterError(s"Type mismatch at '+', " +
              s"unexpected values ${valueToString(leftval)} and ${valueToString(rightval)}", op)
          }
        case MinusBinOp() =>
          (leftval, rightval) match {
            case (IntVal(v1), IntVal(v2)) => IntVal(v1 - v2)
            case (FloatVal(v1), FloatVal(v2)) => FloatVal(v1 - v2)
            case (IntVal(v1), FloatVal(v2)) => FloatVal(v1 - v2)
            case (FloatVal(v1), IntVal(v2)) => FloatVal(v1 - v2)
            case _ => throw InterpreterError(s"Type mismatch at '-', " +
              s"unexpected values ${valueToString(leftval)} and ${valueToString(rightval)}", op)
          }
        case MultBinOp() =>
          (leftval, rightval) match {
            case (IntVal(v1), IntVal(v2)) => IntVal(v1 * v2)
            case (FloatVal(v1), FloatVal(v2)) => FloatVal(v1 * v2)
            case (IntVal(v1), FloatVal(v2)) => FloatVal(v1 * v2)
            case (FloatVal(v1), IntVal(v2)) => FloatVal(v1 * v2)
            case _ => throw InterpreterError(s"Type mismatch at '*', " +
              s"unexpected values ${valueToString(leftval)} and ${valueToString(rightval)}", op)
          }
        case DivBinOp() =>
          if (rightval == IntVal(0) || rightval == FloatVal(0.0f))
            throw InterpreterError(s"Division by zero", op)
          (leftval, rightval) match {
            case (IntVal(v1), IntVal(v2)) => IntVal(v1 / v2)
            case (FloatVal(v1), FloatVal(v2)) => FloatVal(v1 / v2)
            case (IntVal(v1), FloatVal(v2)) => FloatVal(v1 / v2)
            case (FloatVal(v1), IntVal(v2)) => FloatVal(v1 / v2)
            case _ => throw InterpreterError(s"Type mismatch at '/', " +
              s"unexpected values ${valueToString(leftval)} and ${valueToString(rightval)}", op)
          }
        case ModuloBinOp() =>
          if (rightval == IntVal(0) || rightval == FloatVal(0.0f))
            throw InterpreterError(s"Modulo by zero", op)
          (leftval, rightval) match {
            case (IntVal(v1), IntVal(v2)) => IntVal(v1 % v2)
            case (FloatVal(v1), FloatVal(v2)) => FloatVal(v1 % v2)
            case (IntVal(v1), FloatVal(v2)) => FloatVal(v1 % v2)
            case (FloatVal(v1), IntVal(v2)) => FloatVal(v1 % v2)
            case _ => throw InterpreterError(s"Type mismatch at '%', " +
              s"unexpected values ${valueToString(leftval)} and ${valueToString(rightval)}", op)
          }
        case EqualBinOp() =>
          (leftval, rightval) match {
            case (IntVal(v1), IntVal(v2)) => BoolVal(v1 == v2)
            case (FloatVal(v1), FloatVal(v2)) => BoolVal(v1 == v2)
            case (IntVal(v1), FloatVal(v2)) => BoolVal(v1 == v2)
            case (FloatVal(v1), IntVal(v2)) => BoolVal(v1 == v2)
            case (BoolVal(v1), BoolVal(v2)) => BoolVal(v1 == v2)
            case (StringVal(v1), StringVal(v2)) => BoolVal(v1 == v2)
            case (TupleVal(v1), TupleVal(v2)) => BoolVal(v1 == v2)
            case _ => BoolVal(false)
          }
        case LessThanBinOp() =>
          (leftval, rightval) match {
            case (IntVal(v1), IntVal(v2)) => BoolVal(v1 < v2)
            case (FloatVal(v1), FloatVal(v2)) => BoolVal(v1 < v2)
            case (IntVal(v1), FloatVal(v2)) => BoolVal(v1 < v2)
            case (FloatVal(v1), IntVal(v2)) => BoolVal(v1 < v2)
            case (StringVal(v1), StringVal(v2)) => BoolVal(v1 < v2) // Lexicographic comparison
            case _ => throw InterpreterError(s"Type mismatch at '<', " +
              s"unexpected values ${valueToString(leftval)} and ${valueToString(rightval)}", op)
          }
        case LessThanOrEqualBinOp() =>
          (leftval, rightval) match {
            case (IntVal(v1), IntVal(v2)) => BoolVal(v1 <= v2)
            case (FloatVal(v1), FloatVal(v2)) => BoolVal(v1 <= v2)
            case (IntVal(v1), FloatVal(v2)) => BoolVal(v1 <= v2)
            case (FloatVal(v1), IntVal(v2)) => BoolVal(v1 <= v2)
            case (StringVal(v1), StringVal(v2)) => BoolVal(v1 <= v2) // Lexicographic comparison
            case _ => throw InterpreterError(s"Type mismatch at '<=', " +
              s"unexpected values ${valueToString(leftval)} and ${valueToString(rightval)}", op)
          }
        case MaxBinOp() =>
          (leftval, rightval) match {
            case (IntVal(v1), IntVal(v2)) => IntVal(v1 max v2)
            case (FloatVal(v1), FloatVal(v2)) => FloatVal(v1 max v2)
            case (IntVal(v1), FloatVal(v2)) => FloatVal(if (v1 >= v2) v1 else v2)
            case (FloatVal(v1), IntVal(v2)) => FloatVal(if (v1 >= v2) v1 else v2)
            case _ => throw InterpreterError(s"Type mismatch at 'max', " +
              s"unexpected values ${valueToString(leftval)} and ${valueToString(rightval)}", op)
          }
        case AndBinOp() =>
          (leftval, rightval) match {
            case (BoolVal(v1), BoolVal(v2)) => BoolVal(v1 && v2)
            case _ => throw InterpreterError(s"Type mismatch at '&', " +
              s"unexpected values ${valueToString(leftval)} and ${valueToString(rightval)}", op)
          }
        case OrBinOp() =>
          (leftval, rightval) match {
            case (BoolVal(v1), BoolVal(v2)) => BoolVal(v1 || v2)
            case _ => throw InterpreterError(s"Type mismatch at '|', " +
              s"unexpected values ${valueToString(leftval)} and ${valueToString(rightval)}", op)
          }
      }
      // Doesn't trace the initial expression, only the result and the intermediate steps.
      // This keeps the trace output more concise.
      trace(s"Evaluating (${Unparser.unparse(e)}) to ${valueToString(res)}")
      res
    case UnOpExp(op, exp) =>
      val expval = eval(exp, venv, fenv)
      val res = op match {
        case NegUnOp() =>
          expval match {
            case IntVal(v) => IntVal(-v)
            case FloatVal(v) => FloatVal(-v)
            case _ => throw InterpreterError(s"Type mismatch at '-', unexpected value ${valueToString(expval)}", op)
          }
        case NotUnOp() =>
          expval match {
            case BoolVal(v) => BoolVal(!v)
            case _ => throw InterpreterError(s"Type mismatch at '!', unexpected value ${valueToString(expval)}", op)
          }
      }
      trace(s"Evaluating (${Unparser.unparse(e)}) to ${valueToString(res)}")
      res
    case IfThenElseExp(condexp, thenexp, elseexp) =>
      trace(s"Evaluating if-statement: ${Unparser.unparse(e)}")
      val condval = eval(condexp, venv, fenv)
      checkValueType(condval, Some(BoolType()), e)
      trace(s"If: Type of condition is Bool as expected.")
      val branchString = if condval == BoolVal(true) then "then" else "else"
      trace(s"If: Evaluating $branchString branch")
      if condval == BoolVal(true) then {
        eval(thenexp, venv, fenv)
      } else {
        eval(elseexp, venv, fenv)
      }
    case b@BlockExp(vals, defs, exp) =>
      trace("Opening block")
      var venv1 = venv
      var fenv1 = fenv
      for (d <- vals)
        val (venv2, fenv2) = eval(d, venv1, fenv1, b)
        venv1 = venv2
        fenv1 = fenv2
      for (d <- defs)
        val (venv2, fenv2) = eval(d, venv1, fenv1, b)
        venv1 = venv2
        fenv1 = fenv2
      val result = eval(exp, venv1, fenv1)
      trace("Closing block")
      trace(s"Block evaluated to ${valueToString(result)}")
      result
    case TupleExp(exps) =>
      trace(s"Evaluating tuple: ${Unparser.unparse(e)}")
      var vals = List[Val]()
      for (exp <- exps)
        vals = eval(exp, venv, fenv) :: vals
      TupleVal(vals.reverse)
    case MatchExp(exp, cases) =>
      trace(s"Evaluating match: ${Unparser.unparse(e)}")
      val expval = eval(exp, venv, fenv)
      expval match {
        case TupleVal(vs) =>
          for (c <- cases) {
            trace(s"Matching against case ${Unparser.unparse(c)}")
            if (vs.length == c.pattern.length) {
              val newVenv = venv ++ c.pattern.zip(vs)
              trace(s"Extended environment with: ${c.pattern.zip(vs)}")
              trace(s"Matched. Now evaluating case expression")
              return eval(c.exp, newVenv, fenv)
            }
          }
          throw InterpreterError(s"No case matches value ${valueToString(expval)}", e)
        case _ => throw InterpreterError(s"Tuple expected at match, found ${valueToString(expval)}", e)
      }
    case CallExp(fun, args) =>
      val closure = fenv.getOrElse(fun, throw InterpreterError(s"Unknown function '$fun'", e))
      val argsValues = args.map(arg => eval(arg, venv, fenv))
      if (argsValues.length != closure.params.length) {
        throw InterpreterError(s"Argument length mismatch. Requiring ${argsValues.length} parameters " +
          s"but only found ${closure.params.length} in closure.", e)
      }
      val paramValPairs = closure.params.zip(argsValues)

      var closureVenv1: VarEnv = closure.venv
      for ((funParam, value) <- paramValPairs) {
        closureVenv1 = closureVenv1 + (funParam.x -> value)
      }
      var closureFenv1 = closure.fenv + (fun -> closure)
      for (fDef <- closure.defs) {
        closureFenv1 = closureFenv1 +
          (fDef.fun -> Closure(fDef.params, fDef.optrestype, fDef.body, closureVenv1, closureFenv1, closure.defs))
      }
      eval(closure.body, closureVenv1, closureFenv1)
  }

  /**
   * Evaluates a declaration.
   */
  def eval(d: Decl, venv: VarEnv, fenv: FunEnv, b: BlockExp): (VarEnv, FunEnv) = d match {
    case ValDecl(x, opttype, exp) =>
      val venv1 = venv + (x -> eval(exp, venv, fenv))
      (venv1, fenv)
    case DefDecl(fun, params, optrestype, body) =>
      val fenv1 = fenv + (fun -> Closure(params, optrestype, body, venv, fenv, b.defs))
      (venv, fenv1)
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
