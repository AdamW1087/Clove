package clove.ast

// Setting player sprites
enum SpawnConfig:
  case SetSprite(path: String)
  case SetSpritesheet(path: String, frameWidth: Int, frameHeight: Int)
  case AnimRule(name: String, frames: List[Int], fps: Int, condition: Option[Expr] = None, flipped: Boolean = false)