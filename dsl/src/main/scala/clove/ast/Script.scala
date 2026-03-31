package clove.ast

enum Script:
  case Perform(effect: Effect)
  case Seq(first: Script, second: Script)
  case When(cond: Expr, body: Script)
  case Loop(body: Script)
  case WithHandler(handler: Handler, body: Script)
  case Return(value: Expr)
  case Noop
