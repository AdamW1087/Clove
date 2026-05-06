package clove.dsl

import clove.ast.*

sealed trait Behaviour

object Behaviour:
  case class Basic(handlers: List[Handler] = List.empty) extends Behaviour

  case class Solid(oneWay: Boolean = false) extends Behaviour

  case class Trigger(
    onEnter: Script = Script(List.empty),
    onExit:  Script = Script(List.empty)
  ) extends Behaviour

case class Region(
  id: Option[String],
  x: Double, y: Double, w: Double, h: Double,
  behaviour: Behaviour,
  condition: Option[Expr] = None,
  colour: Option[(Double, Double, Double)] = None
)