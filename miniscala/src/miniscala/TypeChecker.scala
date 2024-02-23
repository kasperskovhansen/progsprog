package miniscala

import miniscala.Ast.*
import miniscala.Unparser.unparse

/**
 * Type checker for MiniScala.
 */
object TypeChecker {

  type VarTypeEnv = Map[Var, Type]

  def typeCheck(e: Exp, vtenv: VarTypeEnv): Type = e match {
    case IntLit(_) => IntType()
    case BoolLit(_) => BoolType()
    case FloatLit(_) => FloatType()
    case StringLit(_) => StringType()
    case VarExp(x) => vtenv.getOrElse(x, throw TypeError(s"Unknown identifier $x", e))
    case BinOpExp(leftexp, op, rightexp) =>
      val lefttype = typeCheck(leftexp, vtenv)
      val righttype = typeCheck(rightexp, vtenv)
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
      val exptype = typeCheck(exp, vtenv)
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
      val condtype = typeCheck(condexp, vtenv)
      val thentype = typeCheck(thenexp, vtenv)
      val elsetype = typeCheck(elseexp, vtenv)
      if (condtype != BoolType())
        throw TypeError(s"Type mismatch at if condition: Unexpected type ${unparse(condtype)}", e)
      if (thentype != elsetype)
        throw TypeError(s"Type mismatch at if, then and else have different types ${unparse(thentype)} and ${unparse(elsetype)}", e)
      thentype
    case BlockExp(vals, exp) =>
      var vtenv1 = vtenv
      for (d <- vals) {
        val t = typeCheck(d.exp, vtenv1)
        checkTypesEqual(t, d.opttype, d)
        vtenv1 = vtenv1 + (d.x -> d.opttype.getOrElse(t))
      }
      typeCheck(exp, vtenv1)
    case TupleExp(exps) => TupleType(exps.map(e => typeCheck(e, vtenv)))
    case MatchExp(exp, cases) =>
      val exptype = typeCheck(exp, vtenv)
      exptype match {
        case TupleType(ts) =>
          for (c <- cases) {
            if (ts.length == c.pattern.length) {
              val newVtenv = vtenv ++ c.pattern.zip(ts)
              return typeCheck(c.exp, newVtenv)
            }
          }
          throw TypeError(s"No case matches type ${unparse(exptype)}", e)
        case _ => throw TypeError(s"Tuple expected at match, found ${unparse(exptype)}", e)
      }
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
