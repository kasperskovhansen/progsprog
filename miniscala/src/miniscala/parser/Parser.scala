package miniscala.parser

import miniscala.Ast.*
import miniscala.parser.Tokens.*

import java.io.{File, IOException}
import scala.io.Source
import scala.util.parsing.combinator.PackratParsers
import scala.util.parsing.input.*

/**
  * Parser for MiniScala.
  *
  * (You do *not* need to read this code!)
  */
object Parser extends PackratParsers {

  override type Elem = MiniScalaToken

  private lazy val prog: PackratParser[Exp] = phrase { expr }

  private lazy val expr: PackratParser[Exp] =
    infixexpr

  private lazy val infixexpr: PackratParser[Exp] =
    binopexp()

  private lazy val prefixexpr: PackratParser[Exp] =
    unopexp |
      simpleexpr

  private lazy val simpleexpr: PackratParser[Exp] =
    block |
      simpleexpr1

  private lazy val simpleexpr1: PackratParser[Exp] =
    parens |
      literal |
      ident

  private lazy val ident: PackratParser[Exp] =
    identifier ^^ { id => VarExp(id.str).setPos(id.pos) }

  private lazy val parens: PackratParser[Exp] =
    (LEFT_PAREN() ~ expr ~ RIGHT_PAREN()) ^^ { case _ ~ exp ~ _ => exp }

  private lazy val blockel: PackratParser[AstNode] = valdecl

  private lazy val blockelmseq: PackratParser[List[AstNode]] = rep { blockel ~ SEMICOLON() } ^^ (_.map(_._1))

  type BlockTupleType = List[ValDecl]

  private def validBlock[T](l: List[T]): Option[BlockTupleType] = {
    val matchers = List[Function[T, Boolean]](
      { case _: ValDecl => true
      case _ => false
      })
    val (remaining, splits) = matchers.foldLeft((l, List[List[T]]())) {
      case ((list: List[T], outcome: List[List[T]]), matcher) =>
        val sublist = list.takeWhile(elm => matcher(elm))
        (list.drop(sublist.size), sublist :: outcome)
    }
    val items = splits.reverse
    if (remaining.isEmpty) Some(
      items.head.map(_.asInstanceOf[ValDecl])
    )
    else None
  }

  private lazy val block: PackratParser[BlockExp] = positioned {
    ((LEFT_BRACE() ~! blockelmseq ~! expr ~! RIGHT_BRACE()) ^^ {
      case _ ~ l ~ exp ~ _ => validBlock(l).map(t => BlockExp(t, exp)) } filter(_.isDefined)) ^^ {_.get}
  }

  private lazy val valdecl: PackratParser[Decl] = positioned {
    (VVAL() ~! identifier ~! EQ() ~! expr) ^^ {
      case _ ~ id ~ _ ~ exp => ValDecl(id.str, exp) }
  }

  private def binopexp(prec: Int = 3): PackratParser[Exp] =
    if (prec == 0)
      prefixexpr
    else
      binopexp(prec - 1) * { binop(prec) ^^ { op => { (left: Exp, right: Exp) => BinOpExp(left, op, right) } } }

  private lazy val literal: PackratParser[IntLit] = positioned {
    intliteral ^^ { lit => IntLit(lit.i) }
  }

  private lazy val unopexp: PackratParser[Exp] = positioned {
    (unop ~ simpleexpr) ^^ { case op ~ exp => UnOpExp(op, exp) }
  }

  private def binop(prec: Int): PackratParser[BinOp] = positioned {
    prec match {
      case 1 =>
        mult | div | modulo
      case 2 =>
        plus | minus
      case 3 =>
        max
    }
  }

  private lazy val unop: PackratParser[UnOp] = positioned {
    neg
  }

  private lazy val intliteral: PackratParser[INT] = accept("int literal", { case lit: INT => lit })

  private lazy val identifier: PackratParser[IDENTIFIER] = accept("identifier", { case id: IDENTIFIER => id })

  private lazy val plus: PackratParser[BinOp] = OP("+") ^^ { _ => PlusBinOp() }

  private lazy val minus: PackratParser[BinOp] = OP("-") ^^ { _ => MinusBinOp() }

  private lazy val mult: PackratParser[BinOp] = OP("*") ^^ { _ => MultBinOp() }

  private lazy val div: PackratParser[BinOp] = OP("/") ^^ { _ => DivBinOp() }

  private lazy val max: PackratParser[BinOp] = OP("max") ^^ { _ => MaxBinOp() }

  private lazy val modulo: PackratParser[BinOp] = OP("%") ^^ { _ => ModuloBinOp() }

  private lazy val neg: PackratParser[UnOp] = OP("-") ^^ { _ => NegUnOp() }

  private def parseTokens(tokens: Seq[MiniScalaToken]): Exp =
    prog(MiniScalaTokenReader(tokens)) match {
      case p @ (NoSuccess(_, _) | Failure(_, _) | Error(_, _))  => throw SyntaxError(p.next.pos)
      case Success(result, _) => result
    }

  def parse(code: String): Exp =
    parseTokens(Lexer(code))

  def readFile(path: String): String = {
    try {
      val f = Source.fromFile(File(path))
      try {
        f.mkString
      } finally {
        f.close()
      }
    } catch {
      case e: IOException =>
        throw MiniScalaError(s"Unable to read file ${e.getMessage}", NoPosition)
    }
  }

  class SyntaxError(pos: Position) extends MiniScalaError(s"Syntax error", pos)

  private class MiniScalaTokenReader(tokens: Seq[MiniScalaToken]) extends Reader[MiniScalaToken] {

    override def first: MiniScalaToken = tokens.head

    override def atEnd: Boolean = tokens.isEmpty

    override def pos: Position = tokens.headOption.map(_.pos).getOrElse(NoPosition)

    override def rest: Reader[MiniScalaToken] = MiniScalaTokenReader(tokens.tail)
  }
}
