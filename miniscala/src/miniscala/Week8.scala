package miniscala

import miniscala.*
import miniscala.Ast.Id
import miniscala.Interpreter.Val
import miniscala.Week2.VarEnv
import miniscala.parser.Parser

import scala.annotation.tailrec
import scala.collection.immutable.List

object Week8 {

  def main(args: Array[String]): Unit = {
//    // Natural numbers
//    println(Lambda.encode(Ast.IntLit(0)))
//    println(Lambda.encode(Ast.IntLit(1)))
//    println(Lambda.encode(Ast.IntLit(2)))
//    // e == 0
//    println(Lambda.encode(Ast.BinOpExp(Ast.IntLit(1), Ast.EqualBinOp(), Ast.IntLit(0))))
//    // !e
//    println(Lambda.encode(Ast.UnOpExp(Ast.NotUnOp(), Ast.BoolLit(true))))
//    // { val x = e1; e2 }
//    println(Lambda.encode(Ast.BlockExp(List(Ast.ValDecl("x", None, Ast.IntLit(1))), List(), Ast.VarExp("x"))))
//    // Decode boolean
//    println(Lambda.decodeBoolean(Interpreter.eval(Lambda.encode(Ast.BoolLit(true)), Map[Id, Val]())))

    { var z: Int = 0;
      { def increment(count: Int): Unit = {
        var i = 0;
        while(i <= count) {
          z = z + 1;
          i = i + 1
        };
        z
      };
        increment(9)
      }
    }
  }
}
