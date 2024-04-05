package miniscala

import miniscala.Ast.*
import miniscala.Unparser.unparse

import scala.io.StdIn
import scala.collection.immutable.List

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
  case class RefVal(loc: Loc, opttype: Option[Type]) extends Val

  val unitVal: Val = TupleVal(Nil)

  type Env = Map[Id, Val]

  type Sto = Map[Loc, Val]

  type Loc = Int

  def nextLoc(sto: Sto): Loc = sto.size

  /**
   * Evaluates an expression.
   */
  def eval(e: Exp, env: Env, sto: Sto): (Val, Sto) = e match {
    case IntLit(c) => (IntVal(c), sto)
    case BoolLit(c) => (BoolVal(c), sto)
    case FloatLit(c) => (FloatVal(c), sto)
    case StringLit(c) => (StringVal(c), sto)
    case VarExp(x) =>
      env.getOrElse(x, throw InterpreterError(s"Unknown identifier '$x'", e)) match {
        case RefVal(loc, _) => (sto(loc), sto)
        case v: Val => (v, sto)
      }
    case BinOpExp(leftexp, op, rightexp) =>
      val (leftval, sto1) = eval(leftexp, env, sto)
      val (rightval, sto2) = eval(rightexp, env, sto1)
      val res: (Val, Sto) = op match {
        case PlusBinOp() =>
          (leftval, rightval) match {
            case (IntVal(v1), IntVal(v2)) => (IntVal(v1 + v2), sto2)
            case (FloatVal(v1), FloatVal(v2)) => (FloatVal(v1 + v2), sto2)
            case (IntVal(v1), FloatVal(v2)) => (FloatVal(v1 + v2), sto2)
            case (FloatVal(v1), IntVal(v2)) => (FloatVal(v1 + v2), sto2)
            case (StringVal(v1), StringVal(v2)) => (StringVal(v1 + v2), sto2)
            case (StringVal(v1), IntVal(v2)) => (StringVal(v1 + v2.toString), sto2)
            case (StringVal(v1), FloatVal(v2)) => (StringVal(v1 + v2.toString), sto2)
            case (IntVal(v1), StringVal(v2)) => (StringVal(v1.toString + v2), sto2)
            case (FloatVal(v1), StringVal(v2)) => (StringVal(v1.toString + v2), sto2)
            case _ => throw InterpreterError(s"Type mismatch at '+', " +
              s"unexpected values ${valueToString(leftval)} and ${valueToString(rightval)}", op)
          }
        case MinusBinOp() =>
          (leftval, rightval) match {
            case (IntVal(v1), IntVal(v2)) => (IntVal(v1 - v2), sto)
            case (FloatVal(v1), FloatVal(v2)) => (FloatVal(v1 - v2), sto)
            case (IntVal(v1), FloatVal(v2)) => (FloatVal(v1 - v2), sto)
            case (FloatVal(v1), IntVal(v2)) => (FloatVal(v1 - v2), sto)
            case _ => throw InterpreterError(s"Type mismatch at '-', " +
              s"unexpected values ${valueToString(leftval)} and ${valueToString(rightval)}", op)
          }
        case MultBinOp() =>
          (leftval, rightval) match {
            case (IntVal(v1), IntVal(v2)) => (IntVal(v1 * v2), sto)
            case (FloatVal(v1), FloatVal(v2)) => (FloatVal(v1 * v2), sto)
            case (IntVal(v1), FloatVal(v2)) => (FloatVal(v1 * v2), sto)
            case (FloatVal(v1), IntVal(v2)) => (FloatVal(v1 * v2), sto)
            case _ => throw InterpreterError(s"Type mismatch at '*', " +
              s"unexpected values ${valueToString(leftval)} and ${valueToString(rightval)}", op)
          }
        case DivBinOp() =>
          if (rightval == IntVal(0) || rightval == FloatVal(0.0f))
            throw InterpreterError(s"Division by zero", op)
          (leftval, rightval) match {
            case (IntVal(v1), IntVal(v2)) => (IntVal(v1 / v2), sto2)
            case (FloatVal(v1), FloatVal(v2)) => (FloatVal(v1 / v2), sto2)
            case (IntVal(v1), FloatVal(v2)) => (FloatVal(v1 / v2), sto2)
            case (FloatVal(v1), IntVal(v2)) => (FloatVal(v1 / v2), sto2)
            case _ => throw InterpreterError(s"Type mismatch at '/', " +
              s"unexpected values ${valueToString(leftval)} and ${valueToString(rightval)}", op)
          }
        case ModuloBinOp() =>
          if (rightval == IntVal(0) || rightval == FloatVal(0.0f))
            throw InterpreterError(s"Modulo by zero", op)
          (leftval, rightval) match {
            case (IntVal(v1), IntVal(v2)) => (IntVal(v1 % v2), sto)
            case (FloatVal(v1), FloatVal(v2)) => (FloatVal(v1 % v2), sto)
            case (IntVal(v1), FloatVal(v2)) => (FloatVal(v1 % v2), sto)
            case (FloatVal(v1), IntVal(v2)) => (FloatVal(v1 % v2), sto)
            case _ => throw InterpreterError(s"Type mismatch at '%', " +
              s"unexpected values ${valueToString(leftval)} and ${valueToString(rightval)}", op)
          }
        case EqualBinOp() =>
          (leftval, rightval) match {
            case (IntVal(v1), IntVal(v2)) => (BoolVal(v1 == v2), sto)
            case (FloatVal(v1), FloatVal(v2)) => (BoolVal(v1 == v2), sto)
            case (IntVal(v1), FloatVal(v2)) => (BoolVal(v1 == v2), sto)
            case (FloatVal(v1), IntVal(v2)) => (BoolVal(v1 == v2), sto)
            case (BoolVal(v1), BoolVal(v2)) => (BoolVal(v1 == v2), sto)
            case (StringVal(v1), StringVal(v2)) => (BoolVal(v1 == v2), sto)
            case (TupleVal(v1), TupleVal(v2)) => (BoolVal(v1 == v2), sto)
            case _ => (BoolVal(false), sto)
          }
        case LessThanBinOp() =>
          (leftval, rightval) match {
            case (IntVal(v1), IntVal(v2)) => (BoolVal(v1 < v2), sto)
            case (FloatVal(v1), FloatVal(v2)) => (BoolVal(v1 < v2), sto)
            case (IntVal(v1), FloatVal(v2)) => (BoolVal(v1 < v2), sto)
            case (FloatVal(v1), IntVal(v2)) => (BoolVal(v1 < v2), sto)
            case (StringVal(v1), StringVal(v2)) => (BoolVal(v1 < v2), sto) // Lexicographic comparison
            case _ => throw InterpreterError(s"Type mismatch at '<', " +
              s"unexpected values ${valueToString(leftval)} and ${valueToString(rightval)}", op)
          }
        case LessThanOrEqualBinOp() =>
          (leftval, rightval) match {
            case (IntVal(v1), IntVal(v2)) => (BoolVal(v1 <= v2), sto)
            case (FloatVal(v1), FloatVal(v2)) => (BoolVal(v1 <= v2), sto)
            case (IntVal(v1), FloatVal(v2)) => (BoolVal(v1 <= v2), sto)
            case (FloatVal(v1), IntVal(v2)) => (BoolVal(v1 <= v2), sto)
            case (StringVal(v1), StringVal(v2)) => (BoolVal(v1 <= v2), sto) // Lexicographic comparison
            case _ => throw InterpreterError(s"Type mismatch at '<=', " +
              s"unexpected values ${valueToString(leftval)} and ${valueToString(rightval)}", op)
          }
        case MaxBinOp() =>
          (leftval, rightval) match {
            case (IntVal(v1), IntVal(v2)) => (IntVal(v1 max v2), sto)
            case (FloatVal(v1), FloatVal(v2)) => (FloatVal(v1 max v2), sto)
            case (IntVal(v1), FloatVal(v2)) => (FloatVal(if (v1 >= v2) v1 else v2), sto)
            case (FloatVal(v1), IntVal(v2)) => (FloatVal(if (v1 >= v2) v1 else v2), sto)
            case _ => throw InterpreterError(s"Type mismatch at 'max', " +
              s"unexpected values ${valueToString(leftval)} and ${valueToString(rightval)}", op)
          }
        case AndBinOp() =>
          (leftval, rightval) match {
            case (BoolVal(v1), BoolVal(v2)) => (BoolVal(v1 && v2), sto)
            case _ => throw InterpreterError(s"Type mismatch at '&', " +
              s"unexpected values ${valueToString(leftval)} and ${valueToString(rightval)}", op)
          }
        case OrBinOp() =>
          (leftval, rightval) match {
            case (BoolVal(v1), BoolVal(v2)) => (BoolVal(v1 || v2), sto)
            case _ => throw InterpreterError(s"Type mismatch at '|', " +
              s"unexpected values ${valueToString(leftval)} and ${valueToString(rightval)}", op)
          }
      }
      // Doesn't trace the initial expression, only the result and the intermediate steps.
      // This keeps the trace output more concise.
      trace(s"Evaluating (${Unparser.unparse(e)}) to ${valueToString(res._1)}")
      res
    case UnOpExp(op, exp) =>
      val (expval, sto1) = eval(exp, env, sto)
      val res: (Val, Sto) = op match {
        case NegUnOp() =>
          expval match {
            case IntVal(v) => (IntVal(-v), sto1)
            case FloatVal(v) => (FloatVal(-v), sto1)
            case _ => throw InterpreterError(s"Type mismatch at '-', unexpected value ${valueToString(expval)}", op)
          }
        case NotUnOp() =>
          expval match {
            case BoolVal(v) => (BoolVal(!v), sto)
            case _ => throw InterpreterError(s"Type mismatch at '!', unexpected value ${valueToString(expval)}", op)
          }
      }
      trace(s"Evaluating (${Unparser.unparse(e)}) to ${valueToString(res._1)}")
      res
    case IfThenElseExp(condexp, thenexp, elseexp) =>
      trace(s"Evaluating if-statement: ${Unparser.unparse(e)}")
      val condval = eval(condexp, env, sto)
      checkValueType(condval._1, Some(BoolType()), e)
      trace(s"If: Type of condition is Bool as expected.")
      val branchString = if condval == BoolVal(true) then "then" else "else"
      trace(s"If: Evaluating $branchString branch")
      if condval == BoolVal(true) then {
        eval(thenexp, env, sto)
      } else {
        eval(elseexp, env, sto)
      }
    case b @ BlockExp(vals, vars, defs, exps) =>
      trace("Opening block")
      var env1 = env
      var sto1 = sto
      for (d <- vals ++ vars ++ defs) {
        val (env2, sto2) = eval(d, env1, sto1, b)
        env1 = env2
        sto1 = sto2
      }
      var v1 = unitVal
      for (e <- exps) {
        val (v2, sto2) = eval(e, env1, sto1)
        v1 = v2
        sto1 = sto2
      }
      trace("Closing block")
      trace(s"Block evaluated to ${valueToString(v1)}")
      (v1, sto1)
    case TupleExp(exps) =>
      trace(s"Evaluating tuple: ${Unparser.unparse(e)}")
      var (vals, sto1) = (List[Val](), sto)
      for (exp <- exps) {
        val (v, sto2) = eval(exp, env, sto1)
        vals = v :: vals
        sto1 = sto2
      }
      (TupleVal(vals.reverse), sto1)
    case MatchExp(exp, cases) =>
      trace(s"Evaluating match: ${Unparser.unparse(e)}")
      val (expval, sto1) = eval(exp, env, sto)
      expval match {
        case TupleVal(vs) =>
          for (c <- cases) {
            trace(s"Matching against case ${Unparser.unparse(c)}")
            if (vs.length == c.pattern.length) {
              val env1 = env ++ c.pattern.zip(vs)
              trace(s"Extended environment with: ${c.pattern.zip(vs)}")
              trace(s"Matched. Now evaluating case expression")
              return eval(c.exp, env1, sto1)
            }
          }
          throw InterpreterError(s"No case matches value ${valueToString(expval)}", e)
        case _ => throw InterpreterError(s"Tuple expected at match, found ${valueToString(expval)}", e)
      }
    case CallExp(funexp, args) =>
      val closure: ClosureVal = eval(funexp, env, sto) match
        case (cv@ClosureVal(_, _, _, _, _),sto) => cv
        case v@_ => throw InterpreterError(s"$v is not a function", e)
      var env1: Env = closure.env
      val argsValues = args.map(arg => eval(arg, env, sto))
      if (argsValues.length != closure.params.length) {
        throw InterpreterError(s"Argument length mismatch. Requiring ${argsValues.length} parameters " +
          s"but only found ${closure.params.length} in closure.", e)
      }
      val paramValPairs = closure.params.zip(argsValues)

      // Functional approach:
      val closureEnv1 = paramValPairs.foldLeft(closure.env) {
        (acc: Env, funParamVal)
        => {
          checkValueType(funParamVal._2._1, funParamVal._1.opttype, e)
          acc + (funParamVal._1.x -> funParamVal._2._1)
        }
      }
      val closureEnv2 = closure.defs.foldLeft(closureEnv1) { (acc, fDef) =>
        acc +
          (fDef.fun -> ClosureVal(fDef.params, fDef.optrestype, fDef.body, closure.env, closure.defs))
      }
      val result = eval(closure.body, closureEnv2, sto)

      // Replaces:
      //      var closureEnv1: Env = closure.env
      //      for ((funParam, value) <- paramValPairs) {
      //        checkValueType(value, funParam.opttype, e)
      //        closureEnv1 = closureEnv1 + (funParam.x -> value)
      //      }
      //      for (fDef <- closure.defs) {
      //        closureEnv1 = closureEnv1 +
      //          (fDef.fun -> ClosureVal(fDef.params, fDef.optrestype, fDef.body, closure.env, closure.defs))
      //      }
      //      val result = eval(closure.body, closureEnv1)
      checkValueType(result._1, closure.optrestype, e)
      result

    case LambdaExp(params, body) =>
      (ClosureVal(params, None, body, env, List()), sto)
    case AssignmentExp(x, exp) =>
      ???
    case WhileExp(cond, body) =>
      ???
  }

  /**
   * Evaluates a declaration.
   */
  def eval(d: Decl, env: Env, sto: Sto, b: BlockExp): (Env, Sto) = d match {
    case ValDecl(x, opttype, exp) =>
      val (v, sto1) = eval(exp, env, sto)
      val env1 = env + (x -> v)
      (env1, sto1)
    case VarDecl(x, opttype, exp) =>
      ???
    case DefDecl(fun, params, optrestype, body) =>
      val env1 = env + (fun -> ClosureVal(params, optrestype, body, env, b.defs))
      (env1, sto)
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
    case RefVal(loc, _) => s"#$loc" // the resulting string ignores the type annotation
  }

  /**
   * Builds an initial environment, with a value for each free variable in the program.
   */
  def makeInitialEnv(program: Exp): Env = {
    Vars.freeVars(program).foldLeft(Map[Id, Val]()) {
      (acc, x) => {
        acc + (x -> IntVal(StdIn.readInt()))
      }
    }
    //    var env = Map[Id, Val]()
    //    for (x <- Vars.freeVars(program)) {
    //      print(s"Please provide an integer value for the variable $x: ")
    //      env = env + (x -> IntVal(StdIn.readInt()))
    //    }
    //    env
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
