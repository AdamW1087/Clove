package clove.dsl

import clove.ast.*

case class Region(
  x: Double, y: Double, w: Double, h: Double,
  colorR: Double, colorG: Double, colorB: Double,
  handlers: List[Handler]
)