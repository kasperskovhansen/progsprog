package miniscala

import miniscala.Ast.*
import miniscala.Week7._

/**
  * Computation of free variables (or rather, identifiers).
  */
object Vars {

  def freeVars(e: Exp): Set[Id] = e match {
    case _: Literal => makeEmpty[Id]()
    case VarExp(x) => add(makeEmpty[Id](), x)
    case BinOpExp(leftexp, _, rightexp) => union(freeVars(leftexp), freeVars(rightexp))
    case UnOpExp(_, exp) => freeVars(exp)
    case IfThenElseExp(condexp, thenexp, elseexp) => union(union(freeVars(condexp), freeVars(thenexp)), freeVars(elseexp))
    case BlockExp(vals, defs, exp) =>
      var fv = freeVars(exp)
      for (d <- defs)
        fv = union(fv, freeVars(d))
      for (d <- defs)
        fv = difference(fv, declaredVars(d))
      for (d <- vals.reverse)
        fv = union(difference(fv, declaredVars(d)), freeVars(d))
      fv
    case TupleExp(exps) =>
      var fv = makeEmpty[Id]()
      for (exp <- exps)
        fv = union(fv, freeVars(exp))
      fv
    case MatchExp(exp, cases) =>
      var fv = freeVars(exp)
      for (c <- cases)
        fv = union(fv, difference(freeVars(c.exp), fromScalaList(c.pattern)))
      fv
    case CallExp(funexp, args) =>
      // note: the first field, funexp, is now an expression!
      var fv = freeVars(funexp)
      for (arg <- args)
        fv = union(fv, freeVars(arg))
      fv
    case LambdaExp(params, body) => difference(freeVars(body), fromScalaList(params.map(p => p.x)))
  }

  def freeVars(decl: Decl): Set[Id] = decl match {
    case ValDecl(_, _, exp) => freeVars(exp)
    case DefDecl(_, params, _, body) => difference(freeVars(body), fromScalaList(params.map(p => p.x)))
  }

  def declaredVars(decl: Decl): Set[Id] = decl match {
    case ValDecl(x, _, _) => add(makeEmpty[Id](), x)
    case DefDecl(x, _, _, _) => add(makeEmpty[Id](), x)
  }
}
