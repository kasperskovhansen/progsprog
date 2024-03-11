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

  case class ClosureVal(params: List[FunParam], optrestype: Option[Type], body: Exp, env: Env, defs: List[DefDecl]) extends Val

  type Env = Map[Id, Val]

  /**
   * Evaluates an expression.
   */
  def eval(e: Exp, env: Env): Val = e match {
    case IntLit(c) => IntVal(c)
    case BoolLit(c) => BoolVal(c)
    case FloatLit(c) => FloatVal(c)
    case StringLit(c) => StringVal(c)
    case VarExp(x) =>
      env.getOrElse(x, throw InterpreterError(s"Unknown identifier '$x'", e))
    case BinOpExp(leftexp, op, rightexp) =>
      val leftval = eval(leftexp, env)
      val rightval = eval(rightexp, env)
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
      val expval = eval(exp, env)
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
      val condval = eval(condexp, env)
      checkValueType(condval, Some(BoolType()), e)
      trace(s"If: Type of condition is Bool as expected.")
      val branchString = if condval == BoolVal(true) then "then" else "else"
      trace(s"If: Evaluating $branchString branch")
      if condval == BoolVal(true) then {
        eval(thenexp, env)
      } else {
        eval(elseexp, env)
      }
    case b@BlockExp(vals, defs, exp) =>
      trace("Opening block")
      var env1 = env
      for (d <- vals ++ defs)
        env1 = eval(d, env1, b)
      val result = eval(exp, env1)
      trace("Closing block")
      trace(s"Block evaluated to ${valueToString(result)}")
      result
    case TupleExp(exps) =>
      trace(s"Evaluating tuple: ${Unparser.unparse(e)}")
      var vals = List[Val]()
      for (exp <- exps)
        vals = eval(exp, env) :: vals
      TupleVal(vals.reverse)
    case MatchExp(exp, cases) =>
      trace(s"Evaluating match: ${Unparser.unparse(e)}")
      val expval = eval(exp, env)
      expval match {
        case TupleVal(vs) =>
          for (c <- cases) {
            trace(s"Matching against case ${Unparser.unparse(c)}")
            if (vs.length == c.pattern.length) {
              val env1 = env ++ c.pattern.zip(vs)
              trace(s"Extended environment with: ${c.pattern.zip(vs)}")
              trace(s"Matched. Now evaluating case expression")
              return eval(c.exp, env1)
            }
          }
          throw InterpreterError(s"No case matches value ${valueToString(expval)}", e)
        case _ => throw InterpreterError(s"Tuple expected at match, found ${valueToString(expval)}", e)
      }
    case CallExp(funexp, args) =>
      val closure: ClosureVal = eval(funexp, env) match
        case cv@ClosureVal(_, _, _, _, _) => cv
        case v@_ => throw InterpreterError(s"$v is not a function", e)
      var env1: Env = closure.env
      val argsValues = args.map(arg => eval(arg, env))
      if (argsValues.length != closure.params.length) {
        throw InterpreterError(s"Argument length mismatch. Requiring ${argsValues.length} parameters " +
          s"but only found ${closure.params.length} in closure.", e)
      }
      val paramValPairs = closure.params.zip(argsValues)
      var closureEnv1: Env = closure.env
      for ((funParam, value) <- paramValPairs) {
        checkValueType(value, funParam.opttype, e)
        closureEnv1 = closureEnv1 + (funParam.x -> value)
      }
      for (fDef <- closure.defs) {
        closureEnv1 = closureEnv1 +
          (fDef.fun -> ClosureVal(fDef.params, fDef.optrestype, fDef.body, closure.env, closure.defs))
      }
      val result = eval(closure.body, closureEnv1)
      checkValueType(result, closure.optrestype, e)
      result

    case LambdaExp(params, body) =>
      ClosureVal(params, None, body, env, List())
  }

  /**
   * Evaluates a declaration.
   */
  def eval(d: Decl, env: Env, b: BlockExp): Env = d match {
    case ValDecl(x, opttype, exp) =>
      env + (x -> eval(exp, env))
    case DefDecl(fun, params, optrestype, body) =>
      val env1 = env + (fun -> ClosureVal(params, optrestype, body, env, b.defs))
      env1
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
        case (ClosureVal(cparams, optcrestype, _, _, _), FunType(paramtypes, restype)) if cparams.length == paramtypes.length =>
          for ((p, t) <- cparams.zip(paramtypes))
            checkTypesEqual(t, p.opttype, n)
          checkTypesEqual(restype, optcrestype, n)
        case _ =>
          throw InterpreterError(s"Type mismatch: value ${valueToString(v)} does not match type ${unparse(t)}", n)
      }
    case None => // do nothing
  }

  /**
   * Checks that the types `t1` and `ot2` are equal (if present), throws type error exception otherwise.
   */
  def checkTypesEqual(t1: Type, ot2: Option[Type], n: AstNode): Unit = ot2 match {
    case Some(t2) =>
      if (t1 != t2)
        throw InterpreterError(s"Type mismatch: type ${unparse(t1)} does not match type ${unparse(t2)}", n)
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
    case ClosureVal(params, _, exp, _, _) => // the resulting string ignores the result type annotation and the declaration environment
      s"<(${params.map(unparse).mkString(",")}), ${unparse(exp)}>"
  }

  /**
   * Builds an initial environment, with a value for each free variable in the program.
   */
  def makeInitialEnv(program: Exp): Env = {
    var env = Map[Id, Val]()
    for (x <- Vars.freeVars(program)) {
      print(s"Please provide an integer value for the variable $x: ")
      env = env + (x -> IntVal(StdIn.readInt()))
    }
    env
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
