package clove.dsl

import clove.ast.*

// A handler entry can be a value only, or a value with an impl override
sealed trait HandlerDirective
object HandlerDirective:
  case class ValueOnly(key: String, value: Expr) extends HandlerDirective
  case class Combined(key: String, value: Expr, impl: (Expr, Expr) => Script) extends HandlerDirective

extension (entry: (String, Expr))
  def via(f: (Expr, Expr) => Script): HandlerDirective =
    HandlerDirective.Combined(entry._1, entry._2, f)

// Convert a plain (String, Expr) pair to a ValueOnly directive
given Conversion[(String, Expr), HandlerDirective] = (k, v) =>
  HandlerDirective.ValueOnly(k, v)

given Conversion[(String, Double), HandlerDirective] = (k, v) =>
  HandlerDirective.ValueOnly(k, Expr.Num(v))

given Conversion[(String, Boolean), HandlerDirective] = (k, v) =>
  HandlerDirective.ValueOnly(k, Expr.Bool(v))

given Conversion[(String, String), HandlerDirective] = (k, v) =>
  HandlerDirective.ValueOnly(k, Expr.Str(v))

def createHandler(name: Option[String], directives: Seq[HandlerDirective]): Handler =
  val handles = directives.collect {
    case HandlerDirective.ValueOnly(k, v)   => k -> v
    case HandlerDirective.Combined(k, v, _) => k -> v
  }.toMap
  val impls = directives.collect {
    case HandlerDirective.Combined(k, _, f) => k -> f
  }.toMap
  Handler(name, handles, impls)

def handler(directives: HandlerDirective*): Handler =
  createHandler(None, directives)

def handler(name: String, directives: HandlerDirective*): Handler =
  createHandler(Some(name), directives)

def gravityHandler(strength: Double, name: Option[String] = None): Handler =
  createHandler(name, Seq(HandlerDirective.ValueOnly("Gravity", Expr.Num(strength))))

def moveHandler(speed: Double, name: Option[String] = None): Handler =
  createHandler(name, Seq(HandlerDirective.ValueOnly("Move", Expr.Num(speed))))