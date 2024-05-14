package miniscala

import miniscala.AbstractMachine.Executable
import AbstractMachine._

object Validator {
  def validate(instructions: List[Instruction]): Unit = {
    var code = instructions
    var (opheight, envheight) = (0, 0)

    def assertOpHeight(min: Int): Unit = {
      if (opheight < min) throw new Error("Stack underflow op")
    }

    def assertEnvHeight(min: Int): Unit = {
      if (envheight < min) throw new Error("Stack underflow env")
    }

    while (code.nonEmpty) {
      val inst = code.head
      code = code.tail
      inst match
        case Const(c) => opheight += 1
        case Add
             | Sub
             | Mul
             | Div
             | Eq
             | Lt
             | Leq
             | And
             | Or =>
          assertOpHeight(min = 2)
          opheight -= 1
        case Neg | Not =>
          assertOpHeight(min = 1)
        case Dup =>
          assertOpHeight(min = 1)
          opheight += 1
        case Pop =>
          assertOpHeight(min = 1)
          opheight -= 1
        case Branch(thencode, elsecode) =>
          assertOpHeight(min = 1)
          validate(thencode)
          validate(elsecode)
        case Loop(condcode, bodycode) =>
          assertOpHeight(min = 0)
          validate(condcode)
          validate(bodycode)
          opheight += 1
        case EnterScope =>
          assertOpHeight(min = 1)
          opheight -= 1
          envheight += 1
        case EnterScopeDefs(num) =>
          assertOpHeight(min = num)
          opheight -= num
          envheight += num
        case ExitScope(num) =>
          assertEnvHeight(min = num)
          envheight -= num
        case Read(index) =>
          assertEnvHeight(index)
          opheight += 1
        case Alloc =>
          opheight += 1
        case Load =>
          assertOpHeight(min = 1)
        case Store =>
          assertOpHeight(min = 2)
          opheight -= 2
        // case Lambda(freeids, body) => ???
        // case Call(arity, tailcall) => ???
        // case Return => ???
        case _ => ()
    }
    println((opheight, envheight))
    return
  }
}
