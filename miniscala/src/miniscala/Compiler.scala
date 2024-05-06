package miniscala

import miniscala.AbstractMachine.*
import miniscala.Ast.*
import scala.collection.immutable.List

object Compiler {

  def compile(e: Exp): Executable = {

    val True = Const(1)
    val False = Const(0)

    def lookup(x: Id, idstack: List[Id]): IdIndex = {
      // find the position of identifier x in idstack
      val index = idstack.indexOf(x)
      if (index == -1) throw Exception(s"$x not found")
      index
    }

    def compile(e: Exp, idstack: List[Id]): List[Instruction] = e match {
      case IntLit(c) =>
        List(Const(c))
      case BoolLit(true) =>
        ???
      case BoolLit(false) =>
        ???
      case BinOpExp(leftexp, op, rightexp) =>
        compile(leftexp, idstack) ++ compile(rightexp, idstack) ++ List(op match {
          case PlusBinOp() => Add
          case MinusBinOp() => Sub
          case MultBinOp() => Mul
          case DivBinOp() => Div
          case EqualBinOp() => ???
          case LessThanBinOp() => ???
          case LessThanOrEqualBinOp() => ???
          case AndBinOp() => ???
          case OrBinOp() => ???
          case _ => throw UnsupportedFeatureError(e)
        })
      case UnOpExp(op, exp) =>
        compile(exp, idstack) ++ List(op match {
          case NegUnOp() => Neg
          case NotUnOp() => ???
        })
      case IfThenElseExp(condexp, thenexp, elseexp) =>
        compile(condexp, idstack) ++ List(Branch(compile(thenexp, idstack), compile(elseexp, idstack)))
      case BlockExp(vals, Nil, Nil, Nil, List(exp)) =>
        ???
      case VarExp(x) =>
        ???
      case _ => throw UnsupportedFeatureError(e)
    }

    val freeids = Vars.freeVars(e).toList.sorted
    Executable(freeids, compile(e, freeids))
  }

  class UnsupportedFeatureError(node: AstNode) extends MiniScalaError(s"Sorry, I don't know how to compile $node", node.pos)
}
