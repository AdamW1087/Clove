package clove.ast

enum Expr:
  case Num(value: Double)
  case Str(value: String)
  case Bool(value: Boolean)
  case Var(name: String)
  case BinOp(op: String, left: Expr, right: Expr)
  case Not(expr: Expr)
  case KeyDown(key: String)

object Expr:
  given Conversion[Double, Expr] = Expr.Num(_)
  given Conversion[String, Expr] = Expr.Str(_)
  given Conversion[Boolean, Expr] = Expr.Bool(_)