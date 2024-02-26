package miniscala

import miniscala.Ast.*
import miniscala.Unparser.unparse

/**
 * Type checker for MiniScala.
 */
object TypeChecker {

  type VarTypeEnv = Map[Var, Type]

  type FunTypeEnv = Map[Fun, (List[Type], Type)]

  def typeCheck(e: Exp, vtenv: VarTypeEnv, ftenv: FunTypeEnv): Type = e match {
    case IntLit(_) => IntType()
    case BoolLit(_) => BoolType()
    case FloatLit(_) => FloatType()
    case StringLit(_) => StringType()
    case VarExp(x) => vtenv.getOrElse(x, throw TypeError(s"Unknown identifier $x", e))
    case BinOpExp(leftexp, op, rightexp) =>
      val lefttype = typeCheck(leftexp, vtenv, ftenv)
      val righttype = typeCheck(rightexp, vtenv, ftenv)
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
        case AndBinOp() | OrBinOp() =>
          (lefttype, righttype) match {
            case (BoolType(), BoolType()) => BoolType()
            case _ => throw TypeError(s"Type mismatch at ${unparse(op)}, unexpected types ${unparse(lefttype)} and ${unparse(righttype)}", op)
          }
      }
    case UnOpExp(op, exp) =>
      val exptype = typeCheck(exp, vtenv, ftenv)
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
      val condtype = typeCheck(condexp, vtenv, ftenv)
      val thentype = typeCheck(thenexp, vtenv, ftenv)
      val elsetype = typeCheck(elseexp, vtenv, ftenv)
      if (condtype != BoolType())
        throw TypeError(s"Type mismatch at if condition: Unexpected type ${unparse(condtype)}", e)
      if (thentype != elsetype)
        throw TypeError(s"Type mismatch at if, then and else have different types ${unparse(thentype)} and ${unparse(elsetype)}", e)
      thentype
    case BlockExp(vals, defs, exp) =>
      var tenv = (vtenv, ftenv)
      for (d <- vals) {
        val t = typeCheck(d.exp, tenv._1, tenv._2)
        checkTypesEqual(t, d.opttype, d)
        tenv = (tenv._1 + (d.x -> d.opttype.getOrElse(t)), tenv._2)
      }
      for (d <- defs) {
        val funType = getFunType(d)
        tenv = (tenv._1, tenv._2 + (d.fun -> funType))
        for (p <- d.params) {
          tenv = (tenv._1 + (p.x -> p.opttype.getOrElse(throw TypeError(s"Type annotation missing at parameter ${p.x}", p))), tenv._2)
        }
      }
      for (d <- defs) {
        val funType = getFunType(d)
        checkTypesEqual(funType._2, Some(typeCheck(d.body, tenv._1, tenv._2)), d)
      }
      typeCheck(exp, tenv._1, tenv._2)
    case TupleExp(exps) => TupleType(exps.map(e => typeCheck(e, vtenv, ftenv)))
    case MatchExp(exp, cases) =>
      val exptype = typeCheck(exp, vtenv, ftenv)
      exptype match {
        case TupleType(ts) =>
          for (c <- cases) {
            if (ts.length == c.pattern.length) {
              val newVtenv = vtenv ++ c.pattern.zip(ts)
              return typeCheck(c.exp, newVtenv, ftenv)
            }
          }
          throw TypeError(s"No case matches type ${unparse(exptype)}", e)
        case _ => throw TypeError(s"Tuple expected at match, found ${unparse(exptype)}", e)
      }
    case CallExp(fun, args) =>
      val ftenv1 = ftenv.getOrElse(fun, throw TypeError(s"Unknown identifier $fun", e))
      if (ftenv1._1.length != args.length)
        throw TypeError(s"Wrong number of arguments for function $fun", e)
      val argTypePairs = ftenv1._1.zip(args)
      for ((t, a) <- argTypePairs) {
        val argType = typeCheck(a, vtenv, ftenv)
        checkTypesEqual(t, Some(argType), e)
      }
      ftenv1._2
  }

  /**
    * Returns the parameter types and return type for the function declaration `d`.
    */
  def getFunType(d: DefDecl): (List[Type], Type) =
    (d.params.map(p => p.opttype.getOrElse(throw TypeError(s"Type annotation missing at parameter ${p.x}", p))),
      d.optrestype.getOrElse(throw TypeError(s"Type annotation missing at function result ${d.fun}", d)))

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
  def makeInitialVarTypeEnv(program: Exp): VarTypeEnv = {
    var vtenv: VarTypeEnv = Map()
    for (x <- Vars.freeVars(program))
      vtenv = vtenv + (x -> IntType())
    vtenv
  }

  /**
   * Exception thrown in case of MiniScala type errors.
   */
  class TypeError(msg: String, node: AstNode) extends MiniScalaError(s"Type error: $msg", node.pos)
}
