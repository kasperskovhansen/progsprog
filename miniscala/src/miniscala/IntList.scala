package miniscala

object IntList {

  sealed abstract class IntList {
    override def toString: String = this match
      case Nil => ""
      case Cons(x, Nil) => x.toString
      case Cons(x, xs) => s"$x, $xs"
  }

  case object Nil extends IntList

  case class Cons(x: Int, xs: IntList) extends IntList

}
