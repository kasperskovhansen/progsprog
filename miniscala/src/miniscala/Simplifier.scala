package miniscala

import miniscala.Ast._
import miniscala.parser.Parser
import miniscala.Unparser

object Simplifier {

  def simplify(exp: Exp): Exp = exp match
    case UnOpExp(NegUnOp(), UnOpExp(NegUnOp(), exp)) => simplify(exp)
    case UnOpExp(NegUnOp(), IntLit(a)) if a == 0 => IntLit(a)
    case UnOpExp(op, exp) => UnOpExp(op, simplify(exp))
    case BinOpExp(leftexp, op, rightexp) =>
      val leftSimp = simplify(leftexp)
      val rightSimp = simplify(rightexp)
      val default = BinOpExp(leftSimp, op, rightSimp)
      op match
        case PlusBinOp() => (leftSimp, rightSimp) match
          case (IntLit(a), IntLit(b)) if a == 0 && b == 0 => IntLit(0)
          case (IntLit(a), e) if a == 0 => e
          case (e, IntLit(b)) if b == 0 => e
          case _ => default
        case MinusBinOp() => if (leftSimp == rightSimp) IntLit(0) else default
        case MultBinOp() => (leftSimp, rightSimp) match
          case (IntLit(a), IntLit(b)) if a == 0 || b == 0 => IntLit(0)
          case (IntLit(a), e) if a == 1 => e
          case (e, IntLit(b)) if b == 1 => e
          case _ => default
        case DivBinOp() => (leftSimp, rightSimp) match
          case (IntLit(a), IntLit(b)) if a == 0 && b != 0 => IntLit(0)
          case (e, IntLit(b)) if b == 1 => e
          case _ => default
        case ModuloBinOp() => (leftSimp, rightSimp) match
          case (IntLit(a), IntLit(b)) if a == 0 && b != 0 => IntLit(0)
          case (e, IntLit(b)) if b == 1 => IntLit(0)
          case _ => default
        case MaxBinOp() => (leftSimp, rightSimp) match
          case (IntLit(a), IntLit(b)) if a == b => IntLit(a)
          case _ => default
    case BlockExp(valDecls, exp) => {
      val newValDecls = valDecls.map((decl) => decl match {
        case ValDecl(x, e) => ValDecl(x, simplify(e))
      })
      BlockExp(newValDecls, simplify(exp))
    }
    case _ => exp

  def test(exp: String, simpExp: String): Boolean = {
    println(s"Input: $exp")
    println(s"Expected output: $simpExp")

    val simpliefiedInput = Unparser.unparse(simplify(Parser.parse(exp)))
    println(s"Simplification output: $simpliefiedInput")
    val isCorrect = simpliefiedInput == simpExp
    println((if isCorrect then "Correct" else "Incorrect") + " simplification")
    isCorrect
  }

}
