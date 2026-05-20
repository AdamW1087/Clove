package clove.dsl

import clove.ast.*

// A handler entry can be a value only, or a value with an impl override
sealed trait HandlerDirective
object HandlerDirective:
  case class ValueOnly(key: EffectKey, value: Expr) extends HandlerDirective
  case class Combined(key: EffectKey, value: Expr, impl: (Expr, Expr) => Script) extends HandlerDirective

// Attaches an impl override to a (key -> value) entry
// e.g. Jump -> 5.0 via { (value, dt) => script { ... } }
extension (entry: (EffectKey, Expr))
  def via(f: (Expr, Expr) => Script): HandlerDirective =
    HandlerDirective.Combined(entry._1, entry._2, f)

// Allow plain tuples to be used as handler directives implicitly
given Conversion[(EffectKey, Expr), HandlerDirective]    = (k, v) => HandlerDirective.ValueOnly(k, v)
given Conversion[(EffectKey, Double), HandlerDirective]  = (k, v) => HandlerDirective.ValueOnly(k, Expr.Num(v))
given Conversion[(EffectKey, Boolean), HandlerDirective] = (k, v) => HandlerDirective.ValueOnly(k, Expr.Bool(v))
given Conversion[(EffectKey, String), HandlerDirective]  = (k, v) => HandlerDirective.ValueOnly(k, Expr.Str(v))

// Splits directives into separate value and impl maps for the Handler
def createHandler(name: Option[String], directives: Seq[HandlerDirective]): Handler =
  val handles = directives.collect {
    case HandlerDirective.ValueOnly(k, v)   => k.name -> v
    case HandlerDirective.Combined(k, v, _) => k.name -> v
  }.toMap
  val impls = directives.collect {
    case HandlerDirective.Combined(k, _, f) => k.name -> f
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