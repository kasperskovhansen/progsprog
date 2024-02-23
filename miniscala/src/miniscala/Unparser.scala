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
          case BoolLit(c) => c.toString
          case FloatLit(c) => c.toString
          case StringLit(c) => s"\"$c\""
          case TupleExp(exps) =>
            val result = exps.map(exp => unparse(exp))
            "(" + result.mkString(", ") + ")"
          case VarExp(v) => v
          case BlockExp(vals, funs, exp) =>
            val declarationsString =
              vals.foldLeft("") {
                case (acc, valDecl) => acc + unparse(valDecl) + ";"
              }
            s"{${declarationsString}${unparse(exp)}}"
          case IfThenElseExp(cond, thenExp, elseExp) =>
            s"if (${unparse(cond)}) ${unparse(thenExp)} else ${unparse(elseExp)}"
          case MatchExp(exp, cases) =>
            val casesString = cases.map(c => unparse(c)).mkString("\n")
            s"${unparse(exp)} match {\n$casesString\n}"
        }
      case op: BinOp =>
        op match
          case PlusBinOp() => " + "
          case MinusBinOp() => " - "
          case MultBinOp() => " * "
          case DivBinOp() => " / "
          case EqualBinOp() => " == "
          case LessThanBinOp() => " < "
          case LessThanOrEqualBinOp() => " <= "
          case ModuloBinOp() => " % "
          case MaxBinOp() => " max "
          case AndBinOp() => " & "
          case OrBinOp() => " | "
      case op: UnOp =>
        op match {
          case NegUnOp() => "-"
          case NotUnOp() => "!"
        }
      case decl: Decl =>
        decl match
          case ValDecl(x, t, exp) => s"val $x${unparse(t)} = ${unparse(exp)}"
      case t: Type => t match {
        case IntType() => "Int"
        case BoolType() => "Bool"
        case FloatType() => "Float"
        case StringType() => "String"
        case TupleType(types: List[Type]) =>
          val result = types.map(t => unparse(t))
          "(" + result.mkString(", ") + ")"
      }
      case MatchCase(pattern, exp) =>
        val patternString = pattern.mkString(", ")
        s"case ($patternString) => ${unparse(exp)}"
    }
  } // this unparse function can be used for all kinds of AstNode objects, including Exp objects (see Ast.scala)

  def unparse(ot: Option[Type]): String = ot match {
    case Some(t) => ": " + unparse(t)
    case None => ""
  }
}
