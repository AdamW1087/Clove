package clove.ast

// Contains all Effects/functions to access and manage state
enum Effect:
  // Entity affecting effects
  case Move(dx: Expr, dy: Expr)
  case Jump()
  case Gravity()
  case Despawn()
  case Collides(target: Expr)

  // Effects designed by user affected by handlers (acts on entities)
  case Custom(name: String, impl: (Expr, Expr) => Script = (_, _) => Script(List.empty))
  case Query(name: String)

  // State management (player and global resp)
  case SetState(key: String, value: Expr)
  case GetState(key: String)
  case SetGlobal(key: String, value: Expr)
  case GetGlobal(key: String)
  case SetSize(width: Expr, height: Expr)

  // Others (not sure if classing as 'Effect' is proper
  case Draw()
  case ShowState(key: String)
  case Camera()
