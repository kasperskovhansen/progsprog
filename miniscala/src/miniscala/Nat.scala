package miniscala

import scala.annotation.tailrec

object Nat {

  sealed abstract class Nat
  case object Zero extends Nat
  case class Succ(n: Nat) extends Nat

  @tailrec
  def fold[B](x: Nat, z: B, f: B => B): B = x match
    case Zero => z
    case Succ(n) => fold(n, f(z), f)

}
