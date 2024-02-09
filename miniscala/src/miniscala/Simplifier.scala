package miniscala

import miniscala.Ast.{BinOpExp, Exp, IntLit}
import miniscala.parser.Parser
import miniscala.Unparser

object Simplifier {

  def simplify(exp: Exp): Exp = exp match
    case Ast.UnOpExp(op, unExp) => op match
      case Ast.NegUnOp() => unExp match
        case IntLit(a) if a == 0 => IntLit(a)
        case _ => exp
      case _ => exp
    case Ast.BinOpExp(leftexp, op, rightexp) =>
      val leftSimp = simplify(leftexp)
      val rightSimp = simplify(rightexp)
      op match
        case Ast.PlusBinOp() => (leftSimp, rightSimp) match
          case (IntLit(a), IntLit(b)) if a == 0 && b == 0 => IntLit(0)
          case (IntLit(a), e) if a == 0 => e
          case (e, IntLit(b)) if b == 0 => e
          case _ => exp
        case Ast.MinusBinOp() => if (leftSimp == rightSimp) IntLit(0) else BinOpExp(leftSimp, op, rightSimp)
        case Ast.MultBinOp() => (leftSimp, rightSimp) match
          case (IntLit(a), IntLit(b)) if a == 0 || b == 0 => IntLit(0)
          case (IntLit(a), e) if a == 1 => e
          case (e, IntLit(b)) if b == 1 => e
          case _ => exp
        case Ast.DivBinOp() => (leftSimp, rightSimp) match
          case (IntLit(a), IntLit(b)) if a == 0 && b != 0 => IntLit(0)
          case (e, IntLit(b)) if b == 1 => e
          case _ => exp
        case Ast.ModuloBinOp() => (leftSimp, rightSimp) match
          case (IntLit(a), IntLit(b)) if a == 0 && b != 0 => IntLit(0)
          case (e, IntLit(b)) if b == 1 => IntLit(0)
          case _ => exp
        case Ast.MaxBinOp() => (leftSimp, rightSimp) match
          case (IntLit(a), IntLit(b)) if a == b => IntLit(a)
          case _ => exp
    case _ => exp

  /**
     Simplifier.test
   * Test of the simplification function
   */
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
