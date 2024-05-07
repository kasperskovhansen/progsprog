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
        List(True)
      case BoolLit(false) =>
        List(False)
      case BinOpExp(leftexp, op, rightexp) =>
        compile(leftexp, idstack) ++ compile(rightexp, idstack) ++ List(op match {
          case PlusBinOp() => Add
          case MinusBinOp() => Sub
          case MultBinOp() => Mul
          case DivBinOp() => Div
          case EqualBinOp() => Eq
          case LessThanBinOp() => Lt
          case LessThanOrEqualBinOp() => Leq
          case AndBinOp() => And
          case OrBinOp() => Or
          case _ => throw UnsupportedFeatureError(e)
        })
      case UnOpExp(op, exp) =>
        compile(exp, idstack) ++ List(op match {
          case NegUnOp() => Neg
          case NotUnOp() => Not
        })
      case IfThenElseExp(condexp, thenexp, elseexp) =>
        compile(condexp, idstack) ++ List(Branch(compile(thenexp, idstack), compile(elseexp, idstack)))
      case BlockExp(vals, Nil, Nil, Nil, List(exp)) =>
        def compileVals(vals: List[ValDecl], idstack: List[Id]): (List[Instruction], List[Id]) = {
          if (vals.isEmpty) return (List(), idstack)
          val (tailCompiled, tailIdStack) = compileVals(vals.tail, vals.head.x::idstack)
          (compile(vals.head.exp, idstack) ++ List(EnterScope) ++ tailCompiled, tailIdStack)
        }
        val (instructions, extendedIdStack) = compileVals(vals, idstack)
        instructions ++ compile(exp, extendedIdStack) ++ List(ExitScope(vals.length))
      case VarExp(x) =>
        List(Read(lookup(x, idstack)))
      case _ => throw UnsupportedFeatureError(e)
    }

    val freeids = Vars.freeVars(e).toList.sorted
    Executable(freeids, compile(e, freeids))
  }

  class UnsupportedFeatureError(node: AstNode) extends MiniScalaError(s"Sorry, I don't know how to compile $node", node.pos)
}
