package clove.ast

enum Effect:
  case Move(entity: Expr, dx: Expr, dy: Expr)
  case SetState(entity: Expr, key: String, value: Expr)
  case GetState(entity: Expr, key: String, result: String)
  case Draw(entity: Expr)
  case Spawn(entity: Expr, withGravity: Boolean = false)
  case Despawn(entity: Expr)