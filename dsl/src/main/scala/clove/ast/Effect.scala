package clove.ast

enum Effect:
  case Move(dx: Expr, dy: Expr)
  case Jump()
  case Gravity()
  case Spawn()
  case Despawn()
  case Draw()
  case SetState(key: String, value: Expr)
  case GetState(key: String)
  case Collides(target: Expr)
  case Camera()