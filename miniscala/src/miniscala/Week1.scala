package miniscala

import miniscala.Ast._;
import miniscala.parser.Parser;

object Week1 {
  def main(args: Array[String]): Unit = {

    // Create AST directly
    val a1 = BinOpExp(IntLit(2), MinusBinOp(), IntLit(10))
    assert(Parser.parse(unparse(Parser.parse("(1+2)*3"))) == Parser.parse("(1+2)*3"))

    // Create AST with parser
    val a2 = Parser.parse("2-10")
    println("a2: " + a2)

    // Are they equal?
    println("a1 == a2: " + (a1 == a2))

    // Create complicated AST
    val a3 = Parser.parse("3*(2-10)/6+4")
    println("a3: " + a3)

    // Unparsed
    println("a1 unparsed: " + unparse(a1))
    println("a2 unparsed: " + unparse(a2))
    println("a3 unparsed: " + unparse(a3))

  }

  import miniscala.Ast._;

  def unparse(e: Exp): String = {

    e match {
      case BinOpExp(leftexp, op, rightexp) => {
        val stringBinOp = op match {
          case PlusBinOp() => "+"
          case MinusBinOp() => "-"
          case MultBinOp() => "*"
          case DivBinOp() => "/"
          case ModuloBinOp() => "%"
          case MaxBinOp() => "max"
        }
        "(" + unparse(leftexp) + stringBinOp + unparse(rightexp) + ")"
      }
      case UnOpExp(op, exp) => {
        val stringUnOp = op match {
          case NegUnOp() => "-"
        }
        "(" + stringUnOp + unparse(exp) + ")"
      }
      case IntLit(c) => c.toString
    }
  }

}
