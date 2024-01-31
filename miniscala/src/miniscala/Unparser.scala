package miniscala

import miniscala.Ast.*

/**
 * Unparser for MiniScala.
 */
object Unparser {
  def unparse(n: AstNode): String = {
    n match {
      case exp: Exp =>
        exp match {
          case BinOpExp(leftexp, op, rightexp) =>
            "(" + unparse(leftexp) + unparse(op) + unparse(rightexp) + ")"
          case UnOpExp(op, exp) =>
            "(" + unparse(op) + unparse(exp) + ")"
          case IntLit(c) => c.toString
        }
      case op: BinOp =>
        op match {
          case PlusBinOp() => "+"
          case MinusBinOp() => "-"
          case MultBinOp() => "*"
          case DivBinOp() => "/"
          case ModuloBinOp() => "%"
          case MaxBinOp() => "max"
        }
      case op: UnOp =>
        op match {
          case NegUnOp() => "-"
        }
    }
  } // this unparse function can be used for all kinds of AstNode objects, including Exp objects (see Ast.scala)
}
