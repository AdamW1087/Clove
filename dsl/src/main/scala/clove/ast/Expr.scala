package clove.ast

enum Expr:
  case Num(value: Double)
  case Str(value: String)
  case Bool(value: Boolean)
  case Var(name: String)
  case BinOp(op: String, left: Expr, right: Expr)
  case Not(expr: Expr)
  case KeyDown(key: String)