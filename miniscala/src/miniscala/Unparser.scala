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
          case BlockExp(vals, vars, funs, classes, exps) =>
            val valDeclString =
              vals.foldLeft("") {
                case (acc, valDecl) => acc + unparse(valDecl) + "; "
              }
            val varDeclString =
              vars.foldLeft("") {
                case (acc, varDecl) => acc + unparse(varDecl) + "; "
              }
            val defDeclString =
              funs.foldLeft(varDeclString) {
                case (acc, funDecl) => acc + unparse(funDecl) + "; "
              }
            val classDeclString =
              classes.foldLeft(defDeclString) {
                case (acc, classDecl) => acc + unparse(classDecl) + "; "
              }
            val expsString =
              exps.foldLeft(classDeclString) {
                case (acc, exp) => acc + unparse(exp) + "; "
              }
            s"{$expsString}"
          case IfThenElseExp(cond, thenExp, elseExp) =>
            s"if (${unparse(cond)}) ${unparse(thenExp)} else ${unparse(elseExp)}"
          case MatchExp(exp, cases) =>
            val casesString = cases.map(c => unparse(c)).mkString("\n")
            s"${unparse(exp)} match {\n$casesString\n}"
          case CallExp(fun, args) =>
            val result = args.map(arg => unparse(arg))
            s"${unparse(fun)}(${result.mkString(", ")})"
          case LambdaExp(params, e) =>
            val paramStrings = params.map(p => s"${p.x}${unparse(p.opttype)}")
            s"(${paramStrings.mkString(", ")}) => ${unparse(e)}"
          case AssignmentExp(x, e) =>
            s"$x = ${unparse(e)}"
          case WhileExp(cond, body) =>
            s"while (${unparse(cond)}) ${unparse(body)}"
          case DoWhileExp(body, cond) =>
            s"do ${unparse(body)} while (${unparse(cond)})"
          case NewObjExp(klass, args) =>
            val result = args.map(arg => unparse(arg))
            s"new $klass(${result.mkString(", ")})"
          case LookupExp(obj, field) =>
            s"${unparse(obj)}.$field"


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
          case AndAndBinOp() => " && "
          case OrOrBinOp() => " || "
      case op: UnOp =>
        op match {
          case NegUnOp() => "-"
          case NotUnOp() => "!"
        }
      case decl: Decl =>
        decl match
          case ValDecl(x, t, exp) => s"val $x${unparse(t)} = ${unparse(exp)}"
          case VarDecl(x, t, exp) => s"var $x${unparse(t)} = ${unparse(exp)}"
          case DefDecl(fun, params, t, body) => s"def $fun(${params.map(param => unparse(param)).mkString(", ")})${unparse(t)} = ${unparse(body)}"
          case ClassDecl(name, params, body) => s"class $name(${params.map(param => unparse(param)).mkString(", ")}) { ${unparse(body)} }"
      case t: Type => t match {
        case IntType() => "Int"
        case BoolType() => "Bool"
        case FloatType() => "Float"
        case StringType() => "String"
        case TupleType(types: List[Type]) =>
          val result = types.map(t => unparse(t))
          "(" + result.mkString(", ") + ")"
        case FunType(argTypes: List[Type], restype: Type) =>
          s"${unparse(TupleType(argTypes))} => ${unparse(restype)}"
      }
      case MatchCase(pattern, exp) =>
        val patternString = pattern.mkString(", ")
        s"case ($patternString) => ${unparse(exp)}"
      case FunParam(x, t) => s"$x${unparse(t)}"
      case _ => n.toString
    }
  } // this unparse function can be used for all kinds of AstNode objects, including Exp objects (see Ast.scala)

  def unparse(ot: Option[Type]): String = ot match {
    case Some(t) => ": " + unparse(t)
    case None => ""
  }
}
