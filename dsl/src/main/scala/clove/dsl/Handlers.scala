package clove.dsl

import clove.ast.*

// A handler entry can be a value only, or a value with an impl override
sealed trait HandlerDirective
object HandlerDirective:
  case class ValueOnly(key: EffectKey, value: Expr) extends HandlerDirective
  case class Combined(key: EffectKey, value: Expr, impl: Impl) extends HandlerDirective
  case class ImplOnly(key: EffectKey, impl: Impl) extends HandlerDirective

// Value-effect override: Jump -> 5.0 via { (resolved, dt) => ... }
extension (entry: (EffectKey, Expr))
  def via(f: (Expr, Expr) => Script): HandlerDirective =
    val body = f(Expr.Var("resolved"), Expr.Var("dt"))
    HandlerDirective.Combined(entry._1, entry._2, Impl(body))

// State read override: GetState onGet { (key, dt) => ... }
extension (key: EffectKey)
  def onGet(f: (Expr, Expr) => Script): HandlerDirective =
    val body = f(Expr.Var("key"), Expr.Var("dt"))
    HandlerDirective.ImplOnly(key, Impl(body, List("key")))

  // State write override: SetState onSet { (key, value, dt) => ... }
  def onSet(f: (Expr, Expr, Expr) => Script): HandlerDirective =
    val body = f(Expr.Var("key"), Expr.Var("value"), Expr.Var("dt"))
    HandlerDirective.ImplOnly(key, Impl(body, List("key", "value")))

// Allow plain tuples (Key -> value) to be used as handler directives
given [K <: EffectKey]: Conversion[(K, Expr), HandlerDirective]    = (k, v) => HandlerDirective.ValueOnly(k, v)
given [K <: EffectKey]: Conversion[(K, Double), HandlerDirective]  = (k, v) => HandlerDirective.ValueOnly(k, Expr.Num(v))
given [K <: EffectKey]: Conversion[(K, Boolean), HandlerDirective] = (k, v) => HandlerDirective.ValueOnly(k, Expr.Bool(v))
given [K <: EffectKey]: Conversion[(K, String), HandlerDirective]  = (k, v) => HandlerDirective.ValueOnly(k, Expr.Str(v))

// Splits directives into value and impl maps
def createHandler(name: Option[String], directives: Seq[HandlerDirective]): Handler =
  val handles = directives.collect {
    case HandlerDirective.ValueOnly(k, v)   => k.name -> v
    case HandlerDirective.Combined(k, v, _) => k.name -> v
  }.toMap
  val impls = directives.collect {
    case HandlerDirective.Combined(k, _, i) => k.name -> i
    case HandlerDirective.ImplOnly(k, i)    => k.name -> i
  }.toMap
  Handler(name, handles, impls)

def handler(directives: HandlerDirective*): Handler =
  createHandler(None, directives)

def handler(name: String, directives: HandlerDirective*): Handler =
  createHandler(Some(name), directives)

// Convenience constructors
def gravityHandler(strength: Double, name: Option[String] = None): Handler =
  createHandler(name, Seq(HandlerDirective.ValueOnly(Key.Gravity, Expr.Num(strength))))

def moveHandler(speed: Double, name: Option[String] = None): Handler =
  createHandler(name, Seq(HandlerDirective.ValueOnly(Key.Move, Expr.Num(speed))))