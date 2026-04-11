package clove.dsl

import clove.ast.*

case class CameraConfig(
  follow: Entity,
  threshold: Double,
  axis: Axis
)