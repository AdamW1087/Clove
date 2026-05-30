package clove.ast

enum Expr:
  // Numerical
  case Num(value: Double)
  case Not(expr: Expr)
  case Negate(expr: Expr)
  case Ceil(expr: Expr)
  case Floor(expr: Expr)
  case Max(exprs: Expr*)
  case Min(exprs: Expr*)

  // Other types
  case Str(value: String)
  case Bool(value: Boolean)
  case Var(name: String)
  case BinOp(op: String, left: Expr, right: Expr)

  // User input and propagating values in handlers
  case KeyDown(key: String)
  case JustPressed(key: String)
  case Propagate

  // Direct reads (for animRules and Regions respectively)
  case StateRead(key: String)
  case GlobalRead(key: String)

  // Cross entity direct reads
  case EntityRead(id: String, key: String)
  case EntityExists(id: String)

  // Direct dt access, for timers/cooldowns only, doing physics etc can bypass handlers
  case DeltaTime

object Expr:
  given Conversion[Double, Expr] = Expr.Num(_)
  given Conversion[Int, Expr] = i => Expr.Num(i.toDouble)
  given Conversion[String, Expr] = Expr.Str(_)
  given Conversion[Boolean, Expr] = Expr.Bool(_)
  given Conversion[(String, Double), (String, Expr)] = (k, v) => (k, Expr.Num(v))
  given Conversion[(String, Boolean), (String, Expr)] = (k, v) => (k, Expr.Bool(v))
  given Conversion[(String, String), (String, Expr)] = (k, v) => (k, Expr.Str(v))