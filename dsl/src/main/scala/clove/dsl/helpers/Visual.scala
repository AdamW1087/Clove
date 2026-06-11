package clove.dsl.helpers

import clove.ast.*
import clove.dsl.{VisualMode, Visual, ScriptBuilder}


// Visual helpers

def stretch(path: String): Visual =
  Visual(path, VisualMode.Stretch)

def tile(path: String, size: Double): Visual =
  Visual(path, VisualMode.Tile, Some((size, size)))

def tile(path: String, width: Double, height: Double): Visual =
  Visual(path, VisualMode.Tile, Some((width, height)))

def sprite(path: String): Visual =
  Visual(path, VisualMode.Sprite)

given Conversion[Visual, Option[Visual]] = Some(_)


// Spawn-time configuration

def setSprite(path: String)(using b: ScriptBuilder): Unit =
  b += Configure(SpawnConfig.SetSprite(path))

def setSpritesheet(path: String, frameWidth: Int, frameHeight: Int)(using b: ScriptBuilder): Unit =
  b += Configure(SpawnConfig.SetSpritesheet(path, frameWidth, frameHeight))


// Animation rules

def animRule(name: String, frames: List[Int], fps: Int, flipped: Boolean = false)(cond: Expr)(using b: ScriptBuilder): Unit =
  b += Configure(SpawnConfig.AnimRule(name, frames, fps, condition = Some(cond), flipped))

def animRule(name: String, frames: List[Int], fps: Int, flipped: Boolean)(using b: ScriptBuilder): Unit =
  b += Configure(SpawnConfig.AnimRule(name, frames, fps, condition = None, flipped))