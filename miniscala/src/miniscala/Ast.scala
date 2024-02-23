package miniscala

import scala.util.parsing.input.{NoPosition, Position, Positional}

/**
 * Abstract syntax tree representation of MiniScala programs.
 */
object Ast {

  /**
   * An AST node contains information about its position in the source code.
   */
  sealed abstract class AstNode extends Positional

  /**
    * Identifiers are just strings.
    */
  type Var = String

  /**
    * Common superclass for declarations and expressions.
    */
  sealed abstract class DeclOrExp extends AstNode

  /**
   * Expressions (excluding literals).
   */
  sealed abstract class Exp extends DeclOrExp {
//    def eval(): Int
  }

  case class VarExp(x: Var) extends Exp

  case class BinOpExp(leftexp: Exp, op: BinOp, rightexp: Exp) extends Exp
//  {
//    override def eval(): Int = op.eval(leftexp.eval(), rightexp.eval())
//  }

  case class UnOpExp(op: UnOp, exp: Exp) extends Exp
//  {
//    override def eval(): Int = op.eval(exp.eval())
//  }

  case class IfThenElseExp(condexp: Exp, thenexp: Exp, elseexp: Exp) extends Exp
//  {
//    override def eval(): Int = c
//  }

  case class BlockExp(vals: List[ValDecl], exp: Exp) extends Exp

  case class TupleExp(exps: List[Exp]) extends Exp

  case class MatchExp(exp: Exp, cases: List[MatchCase]) extends Exp

  /**
    * Literals.
    */
  sealed abstract class Literal extends Exp

  case class IntLit(c: Int) extends Literal

  case class BoolLit(c: Boolean) extends Literal

  case class FloatLit(c: Float) extends Literal

  case class StringLit(c: String) extends Literal

  /**
   * Binary operators.
   */
  sealed abstract class BinOp extends AstNode {
//    def eval(leftval: Int, rightval: Int): Int
  }

  case class PlusBinOp() extends BinOp
//  {
//    override def eval(leftval: Int, rightval: Int): Int = leftval + rightval
//  }

  case class MinusBinOp() extends BinOp
//  {
//    override def eval(leftval: Int, rightval: Int): Int = leftval - rightval
//  }

  case class MultBinOp() extends BinOp
//  {
//    override def eval(leftval: Int, rightval: Int): Int = leftval * rightval
//  }

  case class DivBinOp() extends BinOp
//  {
//    override def eval(leftval: Int, rightval: Int): Int = {
//      if (rightval == 0)
//        throw new MiniScalaError("Division by zero", pos)
//      leftval / rightval
//    }
//  }

  case class EqualBinOp() extends BinOp

  case class LessThanBinOp() extends BinOp

  case class LessThanOrEqualBinOp() extends BinOp

  case class ModuloBinOp() extends BinOp
//  {
//    override def eval(leftval: Int, rightval: Int): Int = leftval % rightval
//  }

  case class MaxBinOp() extends BinOp
//  {
//    override def eval(leftval: Int, rightval: Int): Int = Math.max(leftval, rightval)
//  }

  case class AndBinOp() extends BinOp

  case class OrBinOp() extends BinOp

  /**
    * Declarations.
    */
  sealed abstract class Decl extends DeclOrExp

  case class ValDecl(x: Var, opttype: Option[Type], exp: Exp) extends Decl

  /**
    * Match cases.
    */
  case class MatchCase(pattern: List[Var], exp: Exp) extends AstNode

  /**
    * Types.
    */
  abstract class Type extends AstNode

  case class IntType() extends Type

  case class BoolType() extends Type

  case class FloatType() extends Type

  case class StringType() extends Type

  case class TupleType(types: List[Type]) extends Type // Unit is represented as TupleType(Nil)

  case class NotUnOp() extends UnOp

  /**
   * Unary operators.
   */
  sealed abstract class UnOp extends AstNode {
//    def eval(value: Int): Int
  }

  case class NegUnOp() extends UnOp
//  {
//    override def eval(value: Int): Int = -value
//  }

  /**
   * Exception with a message and (optionally) a source code position.
   */
  class MiniScalaError(msg: String, pos: Position = NoPosition)
    extends RuntimeException(if (pos != NoPosition) s"$msg at line ${pos.line} column ${pos.column}" else msg)
}
