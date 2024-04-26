package miniscala

import miniscala.Ast.*
import miniscala.Unparser.unparse

import scala.io.StdIn
import scala.collection.immutable.List
import scala.util.parsing.input.{NoPosition, Position}

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

  case class ClosureVal(params: List[FunParam], optrestype: Option[Type], body: Exp, env: Env, cenv: ClassEnv, defs: List[DefDecl]) extends Val

  case class RefVal(loc: Loc, opttype: Option[Type]) extends Val

  case class ObjRefVal(loc: Loc, opttype: Option[Type]) extends Val

  case class ObjectVal(members: Env) extends Val

  val unitVal: Val = TupleVal(Nil)

  case class Constructor(params: List[FunParam], body: BlockExp, env: Env, cenv: ClassEnv, classes: List[ClassDecl], srcpos: Position)

  case class DynamicClassType(srcpos: Position) extends Type

  type Env = Map[Id, Val]

  type ClassEnv = Map[Id, Constructor]

  type Sto = Map[Loc, Val]

  type Loc = Int

  def nextLoc(sto: Sto): Loc = sto.size

  /**
   * Evaluates an expression.
   */
  def eval(e: Exp, env: Env, cenv: ClassEnv, sto: Sto): (Val, Sto) = e match {
    case IntLit(c) => (IntVal(c), sto)
    case BoolLit(c) => (BoolVal(c), sto)
    case FloatLit(c) => (FloatVal(c), sto)
    case StringLit(c) => (StringVal(c), sto)
    case NullLit() => (ObjRefVal(-1, None), sto)
    case VarExp(x) =>
      (getValue(env.getOrElse(x, throw InterpreterError(s"Unknown identifier '$x'", e)), sto), sto)
    case BinOpExp(leftexp, op, rightexp) =>

      val ((leftval: Val, sto1: Sto),(rightval: Val, sto2: Sto)) = op match {
        case AndAndBinOp() | OrOrBinOp() =>
          ((unitVal, sto), (unitVal, sto))
        case _ =>
          val (tempLeftVal, tempSto1) = eval(leftexp, env, cenv, sto)
          val (tempRightVal, tempSto2) = eval(rightexp, env, cenv, tempSto1)
          ((tempLeftVal, tempSto1), (tempRightVal, tempSto2))
      }

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
            case (ObjRefVal(l1, _), ObjRefVal(l2, _)) => (BoolVal(l1 == l2), sto)
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
        case AndAndBinOp() =>
          val (e1Val, sto1) = eval(leftexp, env, cenv, sto)
          e1Val match {
            case BoolVal(false) =>
              (BoolVal(false), sto1)
            case BoolVal(true) =>
              val (e2Val, sto2) = eval(rightexp, env, cenv, sto1)
              checkValueType(e2Val, Some(BoolType()), e)
              (e2Val, sto2)
            case _ => throw InterpreterError(s"First expression must evaluate to boolean, but got ${valueToString(e1Val)}", e)
          }
        case OrOrBinOp() =>
          val (e1Val, sto1) = eval(leftexp, env, cenv, sto)
          e1Val match {
            case BoolVal(true) =>
              (BoolVal(true), sto1)
            case BoolVal(false) =>
              val (e2Val, sto2) = eval(rightexp, env, cenv, sto1)
              checkValueType(e2Val, Some(BoolType()), e)
              (e2Val, sto2)
            case _ => throw InterpreterError(s"First expression must evaluate to boolean, but got ${valueToString(e1Val)}", e)
          }


      }
      // Doesn't trace the initial expression, only the result and the intermediate steps.
      // This keeps the trace output more concise.
      trace(s"Evaluating (${Unparser.unparse(e)}) to ${valueToString(res._1)}")
      res
    case UnOpExp(op, exp) =>
      val (expval, sto1) = eval(exp, env, cenv, sto)
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
      val (condval, sto1) = eval(condexp, env, cenv, sto)
      checkValueType(condval, Some(BoolType()), e)
      trace(s"If: Type of condition is Bool as expected.")
      val branchString = if condval == BoolVal(true) then "then" else "else"
      trace(s"If: Evaluating $branchString branch")
      if condval == BoolVal(true) then {
        eval(thenexp, env, cenv, sto)
      } else {
        eval(elseexp, env, cenv, sto)
      }
    case b: BlockExp =>
      val (res, _, sto1) = evalBlock(b, env, cenv, sto)
      (res, sto1)
    //    case b@BlockExp(vals, vars, defs, exps) =>
    //      trace("Opening block")
    //      var env1 = env
    //      var sto1 = sto
    //      for (d <- vals ++ vars ++ defs) {
    //        val (env2, sto2) = eval(d, env1, sto1, b)
    //        env1 = env2
    //        sto1 = sto2
    //      }
    //      var v1 = unitVal
    //      for (e <- exps) {
    //        val (v2, sto2) = eval(e, env1, sto1)
    //        v1 = v2
    //        sto1 = sto2
    //      }
    //      trace("Closing block")
    //      trace(s"Block evaluated to ${valueToString(v1)}")
    //      (v1, sto1)
    case TupleExp(exps) =>
      trace(s"Evaluating tuple: ${Unparser.unparse(e)}")
      var (vals, sto1) = (List[Val](), sto)
      for (exp <- exps) {
        val (v, sto2) = eval(exp, env, cenv, sto1)
        vals = v :: vals
        sto1 = sto2
      }
      (TupleVal(vals.reverse), sto1)
    case MatchExp(exp, cases) =>
      trace(s"Evaluating match: ${Unparser.unparse(e)}")
      val (expval, sto1) = eval(exp, env, cenv, sto)
      expval match {
        case TupleVal(vs) =>
          for (c <- cases) {
            trace(s"Matching against case ${Unparser.unparse(c)}")
            if (vs.length == c.pattern.length) {
              val env1 = env ++ c.pattern.zip(vs)
              trace(s"Extended environment with: ${c.pattern.zip(vs)}")
              trace(s"Matched. Now evaluating case expression")
              return eval(c.exp, env1, cenv, sto1)
            }
          }
          throw InterpreterError(s"No case matches value ${valueToString(expval)}", e)
        case _ => throw InterpreterError(s"Tuple expected at match, found ${valueToString(expval)}", e)
      }
    case CallExp(funexp, args) =>
      trace(s"Evaluating call: ${Unparser.unparse(e)}")
      val (closure, sto1): (ClosureVal, Sto) = eval(funexp, env, cenv, sto) match
        case res@(ClosureVal(_, _, _, _, _, _), sto) => res
        case v@_ => throw InterpreterError(s"$v is not a function", e)
      val (env2ext, sto2) = evalArgs(args, closure.params, env, sto1, cenv, closure.env, closure.cenv, e)
      val (val2, sto3) = eval(closure.body, env2ext, closure.cenv, sto2)
      trace("Checking type of result")
      val ot = getType(closure.optrestype, closure.cenv)
      checkValueType(val2, ot, e)
      trace(s"Call evaluated to ${valueToString(val2)}")
      (val2, sto3)

    case LambdaExp(params, body) =>
      (ClosureVal(params, None, body, env, cenv, List()), sto)
    case AssignmentExp(x, exp) =>
      val (v, sto1) = eval(exp, env, cenv, sto)
      val lookup: Val = env.getOrElse(x, throw new InterpreterError(s"Missing identifier: $x", e))
      lookup match
        case RefVal(loc, opttype) =>
          val sto2 = sto1 + (loc -> v)
          checkValueType(v, opttype, e)
          (unitVal, sto2)
        case _ => throw new InterpreterError(s"Variable immutable: $x", e)
    case WhileExp(cond, body) =>
      val (v, sto1) = eval(cond, env, cenv, sto)
      v match
        case BoolVal(true) =>
          val (v2, sto2) = eval(body, env, cenv, sto1)
          eval(e, env, cenv, sto2)
        case BoolVal(false) =>
          (unitVal, sto1)
        case _ =>
          throw new InterpreterError(s"Condition didn't evaluate to a boolean: ${cond}", e)
    case DoWhileExp(body, cond) =>
      val (_, sto1) = eval(body, env, cenv, sto)
      val whileExp = WhileExp(cond, body)
      eval(whileExp, env, cenv, sto1)
    case NewObjExp(klass, args) =>
      trace(s"Making new object: ${Unparser.unparse(e)}")
      val c = cenv.getOrElse(klass, throw InterpreterError(s"Unknown class name '$klass'", e))
      val declcenv1 = rebindClasses(c.env, c.cenv, c.classes)
      val (declenv1, sto1) = evalArgs(args, c.params, env, sto, cenv, c.env, declcenv1, e)
      val (_, env1, sto2) = evalBlock(c.body, declenv1, declcenv1, sto1)
      val newloc = nextLoc(sto2)
      trace(s"Creating new object at location $newloc")
      val objenv = (c.body.defs.map(d => d.fun -> env1(d.fun)) ++ c.body.vars.map(d => d.x -> env1(d.x)) ++ c.body.vals.map(d => d.x -> env1(d.x))).toMap
      val sto3 = sto2 + (newloc -> ObjectVal(objenv))
      trace("Successfully created new object")
      (ObjRefVal(newloc, Some(DynamicClassType(c.srcpos))), sto3)
    case LookupExp(objexp, member) =>
      trace(s"Looking up member $member of object: ${Unparser.unparse(e)}")
      val (objval, sto1) = eval(objexp, env, cenv, sto)
      val (valres, sto2) = objval match {
        case ObjRefVal(-1, _) => throw InterpreterError(s"Null pointer exception when looking up $member", e)
        case ObjRefVal(loc, _) =>
          trace(s"Object value is an object reference, looking up in store")
          sto1(loc) match {
            case ObjectVal(members) =>
              (getValue(members.getOrElse(member, throw InterpreterError(s"No such member: $member", e)), sto1), sto1)
            case _ => throw RuntimeException(s"Expected an object value") // (unreachable case)
          }
        case _ => throw InterpreterError(s"Base value of lookup is not a reference to an object: ${valueToString(objval)}", e)
      }
      trace(s"Lookup of member $member evaluated to ${valueToString(valres)}")
      (valres, sto2)
  }

  /**
   * Evaluates a declaration.
   */
  def eval(d: Decl, env: Env, cenv: ClassEnv, sto: Sto, b: BlockExp): (Env, ClassEnv, Sto) = d match {
    case ValDecl(x, opttype, exp) =>
      val (v, sto1) = eval(exp, env, cenv, sto)
      val ot = getType(opttype, cenv)
      checkValueType(v, ot, d)
      val env1 = env + (x -> v)
      (env1, cenv, sto1)
    case VarDecl(x, opttype, exp) =>
      val (v, sto1) = eval(exp, env, cenv, sto)
      val ot = getType(opttype, cenv)
      val newLoc: Loc = nextLoc(sto1)
      val env1 = env + (x -> RefVal(newLoc, ot))
      val sto2 = sto1 + (newLoc -> v)
      checkValueType(v, ot, d)
      (env1, cenv, sto2)
    case DefDecl(fun, params, optrestype, body) =>
      val env1 = env + (fun -> ClosureVal(params, optrestype, body, env, cenv, b.defs))
      (env1, cenv, sto)
    case ClassDecl(klass, params, body) =>
      val cenv1 = cenv + (klass -> Constructor(params, body, env, cenv, b.classes, d.pos))
      (env, cenv1, sto)
  }

  /**
   * Evaluates the given block.
   * Returns the resulting value, the updated environment after evaluating all declarations, and the latest store.
   */
  def evalBlock(b: BlockExp, env: Env, cenv: ClassEnv, sto: Sto): (Val, Env, Sto) = {
    trace("Opening block")
    var env1 = env
    var cenv1 = cenv
    var sto1 = sto
    for (d <- b.vals ++ b.vars ++ b.defs ++ b.classes) {
      trace(s"Evaluating declaration: ${Unparser.unparse(d)}")
      val (env2, cenv2, sto2) = eval(d, env1, cenv1, sto1, b)
      env1 = env2
      cenv1 = cenv2
      sto1 = sto2
    }
    var v1 = unitVal
    for (e <- b.exps) {
      trace(s"Evaluating expression: ${Unparser.unparse(e)}")
      val (v2, sto2) = eval(e, env1, cenv1, sto1)
      trace(s"Expression evaluated to ${valueToString(v2)}")
      v1 = v2
      sto1 = sto2
    }
    trace("Closing block")
    trace(s"Block evaluated to ${valueToString(v1)}")
    (v1, env1, sto1)
  }

  /**
   * Evaluates the arguments `args` in environment `env` with store `sto`,
   * extends the environment `declenv` with the new bindings, and
   * returns the extended environment and the latest store.
   */
  def evalArgs(args: List[Exp], params: List[FunParam], env: Env, sto: Sto, cenv: ClassEnv, declenv: Env, declcenv: ClassEnv, e: Exp): (Env, Sto) = {
    trace(s"Evaluating arguments: ${args.map(Unparser.unparse).mkString(", ")}")
    if (args.length != params.length) throw InterpreterError("Wrong number of arguments at call/new", e)
    var (env1, sto1) = (declenv, sto)
    for ((p, arg) <- params.zip(args)) {
      trace(s"Evaluating argument: ${Unparser.unparse(arg)}")
      val (argval, sto2) = eval(arg, env, cenv, sto1)
      checkValueType(argval, getType(p.opttype, declcenv), arg)
      env1 = env1 + (p.x -> argval)
      sto1 = sto2
    }
    trace("Evaluated all arguments")
    (env1, sto1)
  }

  /**
   * Resolves reference values by looking up the referenced value in the store.
   */
  def getValue(v: Val, sto: Sto): Val = {
    trace(s"Getting ${valueToString(v)} from store if it is a reference or returning it as is if it is not a reference")
    val res = v match {
      case RefVal(loc, _) =>
        trace(s"Value ${valueToString(v)} is a reference to location $loc in store")
        sto(loc)
      case _ =>
        trace(s"Value ${valueToString(v)} is not a reference, no need to look up in store")
        v
    }
    trace(s"Resolved value to ${valueToString(res)}")
    res
  }

  /**
   * Rebinds `classes` in `cenv` to support recursive class declarations.
   */
  def rebindClasses(env: Env, cenv: ClassEnv, classes: List[ClassDecl]): ClassEnv = {
    trace(s"Rebinding classes for recursive class declarations")
    var cenv1 = cenv
    for (d <- classes)
      trace(s"Rebinding class: ${d.klass}")
      cenv1 = cenv1 + (d.klass -> Constructor(d.params, d.body, env, cenv, classes, d.pos))
    trace("Finished rebinding classes")
    cenv1
  }

  /**
    * Returns the type described by the type annotation `ot` (if present).
    * Class names are converted to proper types according to the class environment `cenv`.
    */
  def getType(ot: Option[Type], cenv: ClassEnv): Option[Type] = ot.map(t => {
    def getType(t: Type): Type = t match {
      case ClassNameType(klass) => DynamicClassType(cenv.getOrElse(klass, throw InterpreterError(s"Unknown class '$klass'", t)).srcpos)
      case IntType() | BoolType() | FloatType() | StringType() | NullType() => t
      case TupleType(ts) => TupleType(ts.map(getType))
      case FunType(paramtypes, restype) => FunType(paramtypes.map(getType), getType(restype))
      case _ => throw RuntimeException(s"Unexpected type $t") // (unreachable case)
    }
    getType(t)
  })

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
        case (ClosureVal(cparams, optcrestype, _, _, cenv, _), FunType(paramtypes, restype)) if cparams.length == paramtypes.length =>
          for ((p, t) <- cparams.zip(paramtypes))
            checkTypesEqual(t, getType(p.opttype, cenv), n)
          checkTypesEqual(restype, getType(optcrestype, cenv), n)
        case (ObjRefVal(-1, _), td: NullType) => // null value
          // do nothing
        case (ObjRefVal(-1, _), td: DynamicClassType) => // null value
          // do nothing
        case (ObjRefVal(_, Some(vd: DynamicClassType)), td: DynamicClassType) =>
          if (vd != td)
            throw InterpreterError(s"Type mismatch: object of type ${unparse(vd)} does not match type ${unparse(td)}", n)
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
    case ClosureVal(params, _, exp, _, _, _) => // the resulting string ignores the result type annotation and the declaration environment
      s"<(${params.map(unparse).mkString(",")}), ${unparse(exp)}>"
    case ObjRefVal(-1, _) => "null"
    case ObjRefVal(loc, _) => s"object#$loc" // the resulting string ignores the type annotation
    case RefVal(loc, _) => s"ref#$loc"
    case _ => throw RuntimeException(s"Unexpected value $v") // (unreachable case)
  }

  /**
   * Builds an initial environment, with a value for each free variable in the program.
   */
  def makeInitialEnv(program: Exp): Env = {
    //    Vars.freeVars(program).foldLeft(Map[Id, Val]()) {
    //      (acc, x) => {
    //        acc + (x -> IntVal(StdIn.readInt()))
    //      }
    //    }
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
