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
  type Id = String

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

  case class VarExp(x: Id) extends Exp

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

  case class BlockExp(vals: List[ValDecl], vars: List[VarDecl], defs: List[DefDecl], classes: List[ClassDecl], exps: List[Exp]) extends Exp

  case class TupleExp(exps: List[Exp]) extends Exp

  case class MatchExp(exp: Exp, cases: List[MatchCase]) extends Exp

  case class CallExp(funexp: Exp, args: List[Exp]) extends Exp

  case class LambdaExp(params: List[FunParam], body: Exp) extends Exp

  case class AssignmentExp(x: Id, exp: Exp) extends Exp

  case class WhileExp(cond: Exp, body: Exp) extends Exp

  case class DoWhileExp(body: Exp, cond: Exp) extends Exp

  case class NewObjExp(klass: Id, args: List[Exp]) extends Exp

  case class LookupExp(objexp: Exp, member: Id) extends Exp

  /**
    * Literals.
    */
  sealed abstract class Literal extends Exp

  case class IntLit(c: Int) extends Literal

  case class BoolLit(c: Boolean) extends Literal

  case class FloatLit(c: Float) extends Literal

  case class StringLit(c: String) extends Literal

  case class NullLit() extends Literal

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

  case class AndAndBinOp() extends BinOp

  case class OrOrBinOp() extends BinOp

  /**
    * Declarations.
    */
  sealed abstract class Decl extends DeclOrExp

  case class ValDecl(x: Id, opttype: Option[Type], exp: Exp) extends Decl

  case class VarDecl(x: Id, opttype: Option[Type], exp: Exp) extends Decl

  case class DefDecl(fun: Id, params: List[FunParam], optrestype: Option[Type], body: Exp) extends Decl

  case class FunType(paramtypes: List[Type], restype: Type) extends Type

  case class ClassDecl(klass: Id, params: List[FunParam], body: BlockExp) extends Decl

  case class ClassNameType(klass: Id) extends Type

  case class NullType() extends Type

  /**
    * Function parameters.
    */
  case class FunParam(x: Id, opttype: Option[Type]) extends AstNode

  /**
    * Match cases.
    */
  case class MatchCase(pattern: List[Id], exp: Exp) extends AstNode

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
