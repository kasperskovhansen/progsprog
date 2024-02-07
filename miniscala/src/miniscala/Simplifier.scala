package miniscala

import miniscala.Ast.{BinOpExp, Exp, IntLit}
import miniscala.parser.Parser
import miniscala.Unparser
object Simplifier {

  def simplify(exp: Exp): Exp = exp match
    case Ast.UnOpExp(op, exp) => ???
    case Ast.BlockExp(vals, exp) => ???
    case Ast.BinOpExp(leftexp, op, rightexp) =>
      val leftSimp = simplify(leftexp)
      val rightSimp = simplify(rightexp)
      op match
        case Ast.PlusBinOp() => (leftSimp, rightSimp) match
          case (IntLit(a), IntLit(b)) if a == 0 && b == 0 => IntLit(0)
          case (IntLit(a), e) if a == 0 => e
          case (e, IntLit(b)) if b == 0 => e
        case Ast.MinusBinOp() => if (leftSimp == rightSimp) IntLit(0) else BinOpExp(leftSimp, op, rightSimp)
        case Ast.MultBinOp() => ???
        case Ast.DivBinOp() => ???
        case Ast.ModuloBinOp() => ???
        case Ast.MaxBinOp() => ???
    case _ => exp

  def test(exp: String, simpExp: String): Boolean ={
    println(s"Input: $exp")
    println(s"Expected output: $simpExp")

    val simpliefiedInput = Unparser.unparse(simplify(Parser.parse(exp)))
    println(s"Simplification output: $simpliefiedInput")
    val isCorrect = simpliefiedInput == simpExp
    println((if isCorrect then "Correct" else "Incorrect") + " simplification")
    isCorrect
  }

}
