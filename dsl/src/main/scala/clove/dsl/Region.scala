package clove.dsl

import clove.ast.*

// Defines how a region acts
sealed trait Behaviour

object Behaviour:
  // Region can contain handlers
  case class Basic(handlers: List[Handler] = List.empty) extends Behaviour

  // Region cannot be walked inside of
  case class Solid(oneWay: Boolean = false) extends Behaviour


case class Region(
  id: String,
  x: Double, y: Double, w: Double, h: Double,
  behaviour: Behaviour,
  condition: Option[Expr] = None,
  colour: Option[(Double, Double, Double)] = None,
  visual: Option[Visual] = None
)