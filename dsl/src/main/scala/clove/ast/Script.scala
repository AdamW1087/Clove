package clove.ast

case class Script(statements: List[Statement])

sealed trait Statement

case class Bind(varName: String, effect: Effect) extends Statement
case class Perform(effect: Effect) extends Statement
case class If(condition: Expr, thenBranch: Script) extends Statement
case class Loop(body: Script) extends Statement // TODO (explicit loops)
case class Return(value: Expr) extends Statement // TODO (early returns)

// TODO: look into setting orderings to have these float to the top
case class HandleWith(handler: Handler, body: Script = Script(List.empty)) extends Statement

case object Noop extends Statement // TODO (empty branches)t