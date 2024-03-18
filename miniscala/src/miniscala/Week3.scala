package miniscala

import miniscala.Ast._
import miniscala.IntList.{Cons, IntList, Nil}
import miniscala.Simplifier.simplify
import miniscala.Unparser.unparse
import miniscala.parser.Parser
import scala.collection.immutable.List

import scala.annotation.tailrec

object Week3 {

  def main(args: Array[String]): Unit = {
    println(Unparser.unparse(IntType()))
    println(Unparser.unparse(FloatType()))
    println(Unparser.unparse(BoolType()))
    println(Unparser.unparse(StringType()))
    println(Unparser.unparse(TupleType(List(IntType(), IntType()))))
    println(Unparser.unparse(TupleType(List(TupleType(List(FloatType(), StringType())), IntType()))))
//    println(Unparser.unparse(BlockExp(
//      List(ValDecl("x", Option(IntType()), IntLit(10)),
//        ValDecl("y", Option(IntType()), IntLit(20))),
//      BinOpExp(IntLit(10), MinusBinOp(), IntLit(20)))))
    println(Unparser.unparse(Parser.parse("{val x: Int = 10; val y: Int = 20; x - y}")))

    println(Unparser.unparse(IntLit(5)))
    println(Unparser.unparse(IntLit(-3)))
    println(Unparser.unparse(BoolLit(true)))
    println(Unparser.unparse(BoolLit(false)))
    println(Unparser.unparse(FloatLit(1.24)))
    println(Unparser.unparse(StringLit("ABC 123")))
    println(Unparser.unparse(TupleExp(List(IntLit(1), StringLit("ABC"), BoolLit(true)))))
    println(Unparser.unparse(Parser.parse("(1, 2) match { case (a, b) => (a < b, a + b) }")))
    println(Unparser.unparse(BinOpExp(IntLit(1), LessThanOrEqualBinOp(), IntLit(2))))
    println(Unparser.unparse(Parser.parse("2 <= 1")))
    println(Unparser.unparse(Parser.parse("true & false")))
    println(Unparser.unparse(Parser.parse("true & true")))
    println(Unparser.unparse(Parser.parse("true | false")))
    println(Unparser.unparse(Parser.parse("5 == 5")))
    println(Unparser.unparse(Parser.parse("5 == 2")))
    println(Unparser.unparse(Parser.parse("!true")))
  }
}

