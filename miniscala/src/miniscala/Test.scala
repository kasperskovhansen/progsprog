package miniscala

import miniscala.Ast.*
import miniscala.Unparser.unparse

object Test {

  def main(args: Array[String]): Unit = {

    // Simple test of the MiniScala unparser
    println(unparse(BinOpExp(
      BinOpExp(
        BinOpExp(
          IntLit(1),
          MinusBinOp(),
          IntLit(2)),
        MultBinOp(),
        IntLit(3)),
      PlusBinOp(),
      IntLit(4))))

  }
}
