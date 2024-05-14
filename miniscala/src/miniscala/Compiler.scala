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
      val bodycode = compile(body, freeidsstack ++ defsstack ++ paramsstack, true) ++ List(Return)
      // find idstack index for each free identifier (excluding defs in same block)
      val indices = freenondefs.map(x => lookup(x, idstack)._1)
      // produce a Lambda instruction
      List(AbstractMachine.Lambda(indices, bodycode))
    }

    def compile(e: Exp, idstack: List[IdDesc], tailpos: Boolean): List[Instruction] =
      e match {
      case IntLit(c) =>
        List(Const(c))
      case BoolLit(true) =>
        List(True)
      case BoolLit(false) =>
        List(False)
      case BinOpExp(leftexp, op, rightexp) =>
        compile(leftexp, idstack, false) ++ compile(rightexp, idstack, false) ++ List(op match {
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
        compile(exp, idstack, false) ++ List(op match {
          case NegUnOp() => Neg
          case NotUnOp() => Not
        })
      case IfThenElseExp(condexp, thenexp, elseexp) =>
        compile(condexp, idstack, false) ++ List(Branch(compile(thenexp, idstack, tailpos), compile(elseexp, idstack, tailpos)))
      case WhileExp(cond, body) =>
        List(Loop(compile(cond, idstack, false), compile(body, idstack, false)), EmptyTuple)
      case BlockExp(vals, vars, defs, Nil, exps) =>
        // Vals
        val (valInstructions, idstack1) = vals.foldLeft((List[Instruction](), idstack)) {
          case ((instrs, ids), valDecl) =>
            (instrs ++ compile(valDecl.exp, ids, false) ++ List(EnterScope),
              IdDesc(valDecl.x, false) :: ids
            )
        }
        // Vars
        val (varInstructions, idstack2) = vars.foldLeft((List[Instruction](), idstack1)) {
          case ((instrs, ids), varDecl) =>
            (instrs ++ List(Alloc, Dup) ++ compile(varDecl.exp, ids, false) ++ List(Store, EnterScope),
              IdDesc(varDecl.x, true) :: ids
            )
        }
        // Defs
        val (defInstructions, idstack3) = defs.foldLeft((List[Instruction](), idstack2)) {
          case ((instrs, ids), defDecl) =>
            (instrs ++ compileFun(defDecl.params, defDecl.body, Vars.freeVars(defDecl).toList.sorted, defs, ids),
              IdDesc(defDecl.fun, false) :: ids
            )
        }
        val tailExp = exps.last
        val expsInstructions = exps.init.foldLeft(List[Instruction]()) {
          (acc, exp) => acc ++ compile(exp, idstack3, false) ++ List(Pop)
        }
        valInstructions ++
          varInstructions ++
          defInstructions ++
          List(EnterScopeDefs(defs.length)) ++
          expsInstructions ++
          compile(tailExp, idstack3, tailpos) ++
          List(ExitScope(vals.length + vars.length + defs.length))
      case VarExp(x) =>
        lookup(x, idstack) match {
          case (index, true) => List(Read(index), Load)
          case (index, false) => List(Read(index))
        }
      case AssignmentExp(x, exp) =>
        val index = lookup(x, idstack)._1
        List(Read(index)) ++ compile(exp, idstack, tailpos) ++ List(Store, EmptyTuple)
      case LambdaExp(params, body) =>
        compileFun(params, body, Vars.freeVars(e).toList.sorted, Nil, idstack)
      case CallExp(funexp, args) =>
        // compile funexp and args, and then add a Call instruction
        compile(funexp, idstack, false) ++ args.flatMap(arg => compile(arg, idstack, false)) ++ List(Call(args.length, tailpos))
      case _ => throw UnsupportedFeatureError(e)
    }

    val freeids = Vars.freeVars(e).toList.sorted
    Executable(freeids, compile(e, freeids.map(x => IdDesc(x, mutable = false)), true))
  }

  class UnsupportedFeatureError(node: AstNode) extends MiniScalaError(s"Sorry, I don't know how to compile $node", node.pos)
}
