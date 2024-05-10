package miniscala

import miniscala.AbstractMachine.*
import miniscala.Ast.*
import scala.collection.immutable.List

object Compiler {

  def compile(e: Exp): Executable = {

    case class IdDesc(x: Id, mutable: Boolean)

    val True = Const(1)
    val False = Const(0)
    val EmptyTuple = Const(0)

    def lookup(x: Id, idstack: List[IdDesc]): (IdIndex, Boolean) = {
      // find the position of identifier x in idstack
      val index = idstack.indexWhere(p => p.x == x)
      if (index == -1) throw Exception(s"$x not found")
      // return the position and a boolean flag that indicates whether the identifier was declared with 'var'
      (index, idstack(index).mutable)
    }

    def compileFun(params: List[FunParam], body: Exp, freeids: List[Id], defs: List[DefDecl], idstack: List[IdDesc]): List[Instruction] = {
      // prepare the new idstack for the function body, with an entry for each free non-def identifier, each def, and each parameter
      val defids = defs.map(d => d.fun).toSet
      val freenondefs = freeids.filterNot(defids.contains)
      val freeidsstack = freenondefs.map(x => IdDesc(x, lookup(x, idstack)._2))
      val defsstack = defs.map(d => IdDesc(d.fun, mutable = false))
      val paramsstack = params.map(p => IdDesc(p.x, mutable = false))
      // compile the function body
      val bodycode = compile(body, freeidsstack ++ paramsstack ++ defsstack, ???) ++ List(Return)
      // find idstack index for each free identifier (excluding defs in same block)
      val indices = freenondefs.map(x => lookup(x, idstack)._1)
      // produce a Lambda instruction
      List(AbstractMachine.Lambda(indices, bodycode))
    }

    def compile(e: Exp, idstack: List[IdDesc], tailpos: Boolean): List[Instruction] = e match {
      case IntLit(c) =>
        List(Const(c))
      case BoolLit(true) =>
        List(True)
      case BoolLit(false) =>
        List(False)
      case BinOpExp(leftexp, op, rightexp) =>
        compile(leftexp, idstack, ???) ++ compile(rightexp, idstack, ???) ++ List(op match {
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
        compile(exp, idstack, ???) ++ List(op match {
          case NegUnOp() => Neg
          case NotUnOp() => Not
        })
      case IfThenElseExp(condexp, thenexp, elseexp) =>
        compile(condexp, idstack, ???) ++ List(Branch(compile(thenexp, idstack, ???), compile(elseexp, idstack, ???)))
      case WhileExp(cond, body) =>
        List(Loop(compile(body, idstack, ???), compile(cond, idstack, ???)), EmptyTuple)
      case BlockExp(vals, vars, defs, Nil, exps) =>
        def compileVals(vals: List[ValDecl], idstack: List[IdDesc]): (List[Instruction], List[IdDesc]) = {
          if (vals.isEmpty) return (List(), idstack)
          val (tailCompiled, tailIdStack) = compileVals(vals.tail, IdDesc(vals.head.x, false)::idstack)
          (compile(vals.head.exp, idstack, false) ++ List(EnterScope) ++ tailCompiled, tailIdStack)
        }
        val (valInstructions, extendedIdStack) = compileVals(vals, idstack)
        val tailExp = exps.last
        val expsInstructions = exps.slice(0, exps.length-1).foldLeft(List[Instruction]()){(acc, exp) => compile(exp, extendedIdStack, false)++acc}
        valInstructions ++ expsInstructions ++ compile(tailExp, extendedIdStack, true) ++ List(ExitScope(vals.length))
      case VarExp(x) =>
        List(Read(lookup(x, idstack)._1))
      case AssignmentExp(x, exp) =>
        ???
      case LambdaExp(params, body) =>
        compileFun(params, body, Vars.freeVars(e).toList.sorted, Nil, idstack)
      case CallExp(funexp, args) =>
        // compile funexp and args, and then add a Call instruction
        compile(funexp, idstack, ???) ++ args.flatMap(arg => compile(arg, idstack, ???)) ++ List(Call(args.length, tailpos))
      case _ => throw UnsupportedFeatureError(e)
    }

    val freeids = Vars.freeVars(e).toList.sorted
    Executable(freeids, compile(e, freeids.map(x => IdDesc(x, mutable = false)), ???))
  }

  class UnsupportedFeatureError(node: AstNode) extends MiniScalaError(s"Sorry, I don't know how to compile $node", node.pos)
}
