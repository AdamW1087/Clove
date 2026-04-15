package clove.ast

enum Expr:
  case Num(value: Double)
  case Str(value: String)
  case Bool(value: Boolean)
  case Var(name: String)
  case BinOp(op: String, left: Expr, right: Expr)
  case Not(expr: Expr)
  case KeyDown(key: String)
  case Propagate(expr: Expr)

object Expr:
  given Conversion[Double, Expr] = Expr.Num(_)
  given Conversion[Int, Expr] = i => Expr.Num(i.toDouble)
  given Conversion[String, Expr] = Expr.Str(_)
  given Conversion[Boolean, Expr] = Expr.Bool(_)
  given Conversion[(String, Double), (String, Expr)] = (k, v) => (k, Expr.Num(v))
  given Conversion[(String, Boolean), (String, Expr)] = (k, v) => (k, Expr.Bool(v))
  given Conversion[(String, String), (String, Expr)] = (k, v) => (k, Expr.Str(v))
  extension (e: Expr)
    def *(other: Expr): Expr = Expr.BinOp("*", e, other)
    def +(other: Expr): Expr = Expr.BinOp("+", e, other)
    def -(other: Expr): Expr = Expr.BinOp("-", e, other)
    def /(other: Expr): Expr = Expr.BinOp("/", e, other)