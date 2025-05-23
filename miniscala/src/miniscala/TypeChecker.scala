package miniscala

import miniscala.Ast.*
import miniscala.Unparser.unparse

import scala.util.parsing.input.Position

/**
 * Type checker for MiniScala.
 */
object TypeChecker {

  type TypeEnv = Map[Id, Type]

  type ClassTypeEnv = Map[Id, StaticClassType]

  case class MutableType(thetype: Type) extends Type

  case class StaticClassType(srcpos: Position, params: List[Type], membertypes: TypeEnv, ctenv: ClassTypeEnv, classes: List[ClassDecl]) extends Type

  val unitType: Type = TupleType(Nil)

  def typeCheck(e: Exp, tenv: TypeEnv, ctenv: ClassTypeEnv): Type = e match {
    case IntLit(_) => IntType()
    case BoolLit(_) => BoolType()
    case FloatLit(_) => FloatType()
    case StringLit(_) => StringType()
    case NullLit() => NullType()
    case VarExp(x) => tenv.getOrElse(x, throw TypeError(s"Unknown identifier '$x'", e)) match {
      case MutableType(thetype) => thetype
      case t: Type => t
    }
    case BinOpExp(leftexp, op, rightexp) =>
      val lefttype = typeCheck(leftexp, tenv, ctenv)
      val righttype = typeCheck(rightexp, tenv, ctenv)
      op match {
        case PlusBinOp() =>
          (lefttype, righttype) match {
            case (IntType(), IntType()) => IntType()
            case (FloatType(), FloatType()) => FloatType()
            case (IntType(), FloatType()) => FloatType()
            case (FloatType(), IntType()) => FloatType()
            case (StringType(), StringType()) => StringType()
            case (StringType(), IntType()) => StringType()
            case (StringType(), FloatType()) => StringType()
            case (IntType(), StringType()) => StringType()
            case (FloatType(), StringType()) => StringType()
            case _ => throw TypeError(s"Type mismatch at '+', unexpected types ${unparse(lefttype)} and ${unparse(righttype)}", op)
          }
        case MinusBinOp() | MultBinOp() | DivBinOp() | ModuloBinOp() | MaxBinOp() =>
          (lefttype, righttype) match {
            case (IntType(), IntType()) => IntType()
            case (FloatType(), FloatType()) => FloatType()
            case (IntType(), FloatType()) => FloatType()
            case (FloatType(), IntType()) => FloatType()
            case _ => throw TypeError(s"Type mismatch at '${unparse(op)}', unexpected types ${unparse(lefttype)} and ${unparse(righttype)}", op)
          }
        case EqualBinOp() =>
          (lefttype, righttype) match {
            case (IntType(), IntType()) => BoolType()
            case (FloatType(), FloatType()) => BoolType()
            case (IntType(), FloatType()) => BoolType()
            case (FloatType(), IntType()) => BoolType()
            case (StringType(), StringType()) => BoolType()
            case (BoolType(), BoolType()) => BoolType()
            case (TupleType(_), TupleType(_)) => BoolType()
            case _ => throw TypeError(s"Type mismatch at '==', unexpected types ${unparse(lefttype)} and ${unparse(righttype)}", op)
          }
        case LessThanBinOp() | LessThanOrEqualBinOp() =>
          (lefttype, righttype) match {
            case (IntType(), IntType()) => BoolType()
            case (FloatType(), FloatType()) => BoolType()
            case (IntType(), FloatType()) => BoolType()
            case (FloatType(), IntType()) => BoolType()
            case (StringType(), StringType()) => BoolType()
            case _ => throw TypeError(s"Type mismatch at ${unparse(op)}, unexpected types ${unparse(lefttype)} and ${unparse(righttype)}", op)
          }
        case AndBinOp() | OrBinOp() | AndAndBinOp() | OrOrBinOp() =>
          (lefttype, righttype) match {
            case (BoolType(), BoolType()) => BoolType()
            case _ => throw TypeError(s"Type mismatch at ${unparse(op)}, unexpected types ${unparse(lefttype)} and ${unparse(righttype)}", op)
          }
      }
    case UnOpExp(op, exp) =>
      val exptype = typeCheck(exp, tenv, ctenv)
      op match {
        case NegUnOp() =>
          exptype match {
            case IntType() => IntType()
            case FloatType() => FloatType()
            case _ => throw TypeError(s"Type mismatch at ${unparse(op)}, unexpected type ${unparse(exptype)}", op)
          }
        case NotUnOp() =>
          exptype match {
            case BoolType() => BoolType()
            case _ => throw TypeError(s"Type mismatch at ${unparse(op)}, unexpected type ${unparse(exptype)}", op)
          }
      }
    case IfThenElseExp(condexp, thenexp, elseexp) =>
      val condtype = typeCheck(condexp, tenv, ctenv)
      val thentype = typeCheck(thenexp, tenv, ctenv)
      val elsetype = typeCheck(elseexp, tenv, ctenv)
      if (!subtype(condtype, BoolType()))
        throw TypeError(s"Type mismatch at if condition: Unexpected type ${unparse(condtype)}", e)
      if (!subtype(thentype, elsetype))
        throw TypeError(s"Type mismatch at if, then and else have different types ${unparse(thentype)} and ${unparse(elsetype)}", e)
      thentype
    case BlockExp(vals, vars, defs, classes, exps) =>
      var tenv1 = tenv
      for (d <- vals) {
        val t = typeCheck(d.exp, tenv1, ctenv)
        val ot = getType(d.opttype, ctenv)
        checkSubtype(t, ot, d)
        tenv1 = tenv1 + (d.x -> ot.getOrElse(t))
      }
      for (d <- vars) {
        val t = typeCheck(d.exp, tenv1, ctenv)
        val ot = getType(d.opttype, ctenv)
        checkSubtype(t, ot, d)
        tenv1 = tenv1 + (d.x -> MutableType(d.opttype.getOrElse(t)))
      }
      for (d <- defs) {
        val funType = getType(makeFunType(d), ctenv)
        tenv1 = tenv1 + (d.fun -> funType)
      }
      for (d <- defs) {
        var tenv2 = tenv1
        for (p <- d.params) {
          tenv2 = tenv2 + (p.x -> getType(p.opttype.getOrElse(throw TypeError(s"Type annotation missing at parameter ${p.x}", p)), ctenv))
        }
        val funType = makeFunType(d)
        checkSubtype(getType(funType._2, ctenv), Some(typeCheck(d.body, tenv2, ctenv)), d)
      }
      val ctenv1 = rebindClasses(ctenv, classes)
      for (c <- classes) {
        var tenv2 = tenv1
        for (p <- c.params) {
          tenv2 = tenv2 + (p.x -> getType(p.opttype.getOrElse(throw TypeError(s"Type annotation missing at parameter ${p.x}", p)), ctenv1))
        }
        typeCheck(c.body, tenv2, ctenv1)
      }
      var resType: Type = unitType
      for (exp: Exp <- exps) {
        resType = typeCheck(exp, tenv1, ctenv1)
      }
      resType
    case TupleExp(exps) => TupleType(exps.map(e => typeCheck(e, tenv, ctenv)))
    case MatchExp(exp, cases) =>
      val exptype = typeCheck(exp, tenv, ctenv)
      exptype match {
        case TupleType(ts) =>
          for (c <- cases) {
            if (ts.length == c.pattern.length) {
              val tenv1 = tenv ++ c.pattern.zip(ts)
              return typeCheck(c.exp, tenv1, ctenv)
            }
          }
          throw TypeError(s"No case matches type ${unparse(exptype)}", e)
        case _ => throw TypeError(s"Tuple expected at match, found ${unparse(exptype)}", e)
      }
    case CallExp(funexp, args) =>
      val fun = typeCheck(funexp, tenv, ctenv) match
        case ft@FunType(_, _) => ft
        case t@_ => throw TypeError(s"$t is not a closure", e)
      if (fun.paramtypes.length != args.length)
        throw TypeError(s"Wrong number of arguments for function $fun", e)
      val argTypePairs = fun.paramtypes.zip(args)
      for ((t, a) <- argTypePairs) {
        val argType = typeCheck(a, tenv, ctenv)
        checkSubtype(argType, t, e)
      }
      fun.restype
    case LambdaExp(params, body) =>
      var tenv1 = tenv
      for (p <- params) {
        tenv1 = tenv1 + (p.x -> p.opttype.getOrElse(throw TypeError(s"Type annotation missing at parameter ${p.x}", e)))
      }
      val returnType = typeCheck(body, tenv1, ctenv)
      makeFunType(DefDecl("_", params, Some(returnType), body))
    case AssignmentExp(x, exp) =>
      tenv.getOrElse(x, throw TypeError(s"Unknown identifier '$x'", e)) match {
        case MutableType(thetype) =>
          val tau = typeCheck(exp, tenv, ctenv)
          checkSubtype(tau, Some(thetype), e)
          unitType
        case t: Type => throw TypeError(s"Assignment to immutable variable: $x", e)
      }
    case WhileExp(cond, body) =>
      val tau = typeCheck(cond, tenv, ctenv)
      tau match {
        case BoolType() =>
          typeCheck(body, tenv, ctenv)
          unitType
        case _ => throw TypeError(s"Condition must be of type boolean: $cond", e)
      }
    case DoWhileExp(body, cond) =>
      typeCheck(body, tenv, ctenv)
      val e2Type = typeCheck(cond, tenv, ctenv)
      e2Type match {
        case BoolType() => unitType
        case _ => throw TypeError(s"Do-while condition must be of type boolean: $cond", e)
      }
    case NewObjExp(klass, args) =>
      val sct = ctenv.getOrElse(klass, throw TypeError(s"Unknown class '$klass'", e))
      if (sct.params.length != args.length)
        throw TypeError(s"Wrong number of arguments for class $klass", e)

      val ctenv1 = rebindClasses(sct.ctenv, sct.classes)

      val params = sct.params.map { p => getType(p, ctenv1) }

      val argTypePairs = params.zip(args)

      for ((t, a) <- argTypePairs) {
        val argType = typeCheck(a, tenv, ctenv1)
        checkSubtype(t, Some(argType), e)
      }
      sct
    case LookupExp(objexp, member) =>
      val objtype = typeCheck(objexp, tenv, ctenv)
      val sct = objtype match {
        case sct: StaticClassType => sct
        case _ => throw TypeError(s"Object expected at lookup, found ${unparse(objtype)}", e)
      }
      val ctenv1 = rebindClasses(sct.ctenv, sct.classes)
      val mtypes = sct.membertypes.map { case (k, v) => (k, getType(v, ctenv1)) }
      mtypes.getOrElse(member, throw TypeError(s"Unknown member '$member' in class ${sct}", e))
  }

  /**
   * Returns the type described by the type annotation `t`.
   * Class names are converted to proper types according to the class type environment `ctenv`.
   */
  def getType(t: Type, ctenv: ClassTypeEnv): Type = t match {
    case ClassNameType(klass) => ctenv.getOrElse(klass, throw TypeError(s"Unknown class '$klass'", t))
    case IntType() | BoolType() | FloatType() | StringType() | NullType() => t
    case TupleType(ts) => TupleType(ts.map(tt => getType(tt, ctenv)))
    case FunType(paramtypes, restype) => FunType(paramtypes.map(tt => getType(tt, ctenv)), getType(restype, ctenv))
    case _ => throw RuntimeException(s"Unexpected type ${unparse(t)}") // this case is unreachable...
  }

  /**
   * Returns the type described by the optional type annotation `ot` (if present).
   */
  def getType(ot: Option[Type], ctenv: ClassTypeEnv): Option[Type] = ot.map(t => getType(t, ctenv))

  /**
   * Returns the function type for the function declaration `d`.
   */
  def makeFunType(d: DefDecl): FunType =
    FunType(d.params.map(p => p.opttype.getOrElse(throw TypeError(s"Type annotation missing at parameter ${p.x}", p))),
      d.optrestype.getOrElse(throw TypeError(s"Type annotation missing at function result ${d.fun}", d)))

  /**
   * Returns the class type for the class declaration `d`.
   */
  def makeStaticClassType(d: ClassDecl, ctenv: ClassTypeEnv, classes: List[ClassDecl]): StaticClassType = {
    var membertypes: TypeEnv = Map()
    for (m <- d.body.vals)
      membertypes = membertypes + (m.x -> m.opttype.getOrElse(throw TypeError(s"Type annotation missing at field ${m.x}", m)))
    for (m <- d.body.vars)
      membertypes = membertypes + (m.x -> m.opttype.getOrElse(throw TypeError(s"Type annotation missing at field ${m.x}", m)))
    for (m <- d.body.defs)
      membertypes = membertypes + (m.fun -> makeFunType(m))
    StaticClassType(d.pos, d.params.map(f => f.opttype.getOrElse(throw TypeError(s"Type annotation missing at parameter ${f.x}", d))), membertypes, ctenv, classes)
  }

  /**
   * Checks that the types `t1` and `ot2` are equal (if present), throws type error exception otherwise.
   */
  def checkTypesEqual(t1: Type, ot2: Option[Type], n: AstNode): Unit = ot2 match {
    case Some(t2) =>
      if (t1 != t2)
        throw TypeError(s"Type mismatch: expected type ${unparse(t2)}, found type ${unparse(t1)}", n)
    case None => // do nothing
  }

  /**
   * Builds an initial type environment, with a type for each free variable in the program.
   */
  def makeInitialTypeEnv(program: Exp): TypeEnv = {
    var tenv: TypeEnv = Map()
    for (x <- Vars.freeVars(program))
      tenv = tenv + (x -> IntType())
    tenv
  }

  /**
   * Checks whether `t1` is a subtype of `t2`.
   */
  def subtype(t1: Type, t2: Type): Boolean = {
    (t1, t2) match {
      case (IntType(), FloatType()) => true
      case (NullType(), StaticClassType(_, _, _, _, _)) => true
      case (StaticClassType(posA, _, _, _, _), StaticClassType(posB, _, _, _, _)) => posA == posB
      case (TupleType(ts1), TupleType(ts2)) => ts1.zip(ts2).forall { (tt1, tt2) => subtype(tt1, tt2) }
      case (FunType(pt1, restype1), FunType(pt2, restype2)) =>
        pt1.zip(pt2).forall {  (tt1, tt2) => subtype(tt2, tt1) } && subtype(restype1, restype2)
      case _ => t1 == t2
    }
  }

  /**
   * Checks whether `t1` is a subtype of `t2`, generates type error otherwise.
   */
  def checkSubtype(t1: Type, t2: Type, n: AstNode): Unit =
    if (!subtype(t1, t2)) throw new TypeError(s"Type mismatch: type ${unparse(t1)} is not subtype of ${unparse(t2)}", n)

  /**
   * Checks whether `t1` is a subtype of `ot2` (if present), generates type error otherwise.
   */
  def checkSubtype(t: Type, ot2: Option[Type], n: AstNode): Unit = ot2 match {
    case Some(t2) => checkSubtype(t, t2, n)
    case None => // do nothing
  }

  /**
   * Rebind classes
   */
  def rebindClasses(ctenv: ClassTypeEnv, classes: List[ClassDecl]): ClassTypeEnv = {
    var ctenv1 = ctenv
    for (c <- classes) {
      val staticClassType = makeStaticClassType(c, ctenv, classes)
      ctenv1 = ctenv1 + (c.klass -> staticClassType)
    }
    ctenv1
  }
  
  /**
   * Exception thrown in case of MiniScala type errors.
   */
  class TypeError(msg: String, node: AstNode) extends MiniScalaError(s"Type error: $msg", node.pos)
}
