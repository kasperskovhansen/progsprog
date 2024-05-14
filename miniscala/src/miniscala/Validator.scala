//package miniscala
//
//import miniscala.AbstractMachine.Executable
//import AbstractMachine._
//
//object Validator {
//  def validate(instructions: List[Instruction]): (Int, Int) = {
//    var code = instructions
//    var (opheight, envheight) = (0, 0)
//    def checkUnderflow(): Unit = {
//      if (opheight < 0 || envheight < 0) {
//        throw new Error("Stack underflow")
//      }
//    }
//
//    // Iterate over instructions
//    while (code.nonEmpty) {
//      val inst = code.head
//      code = code.tail
//      inst match
//        case Const(c) => opheight += 1
//        case AbstractMachine.Add => opheight -= 1
//        case AbstractMachine.Sub => opheight -= 1
//        case AbstractMachine.Mul => opheight -= 1
//        case AbstractMachine.Div => opheight -= 1
//        case AbstractMachine.Eq => opheight -= 1
//        case AbstractMachine.Lt => opheight -= 1
//        case AbstractMachine.Leq => opheight -= 1
//        case AbstractMachine.And => opheight -= 1
//        case AbstractMachine.Or => opheight -= 1
//        case AbstractMachine.Neg => ()
//        case AbstractMachine.Not => ()
//        case AbstractMachine.Dup => opheight += 1
//        case AbstractMachine.Pop => opheight -= 1
//        case Branch(thencode, elsecode) =>
//          val (thenOp, thenEnv) = validate(thencode)
//          val (elseOp, elseEnv) = validate(elsecode)
//          ()
//          //  opheight -= Math.min(thenopheight, elseopheight)
//        case Loop(condcode, bodycode) => ()
//        case AbstractMachine.EnterScope => envheight += 1
//        case EnterScopeDefs(num) => envheight += num
//        case ExitScope(num) => envheight -= num
//        case Read(index) => opheight += 1
//        case AbstractMachine.Alloc => opheight += 1
//        case AbstractMachine.Load => ()
//        case AbstractMachine.Store => opheight -= 2
////        case Lambda(freeids, body) => ???
////        case Call(arity, tailcall) => ???
////        case AbstractMachine.Return => ???
//        case _ => ()
//
//      checkUnderflow()
//
//      return (opheight, envheight)
//    }
//  }
//}
