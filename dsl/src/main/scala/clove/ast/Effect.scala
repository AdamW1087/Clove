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
  case ShowState(key: String)
  case Collides(target: Expr)
  case Camera()
  case Custom(name: String, impl: (Expr, Expr) => Script = (_, _) => Script(List.empty))
  case Query(name: String)
  // TODO: refactor what are effects and what are general operations
  // also todo animated sprites
  case SetSprite(path: String)
  case SetSize(width: Expr, height: Expr)