package clove.ast

case class Script(statements: List[Statement])

sealed trait Statement

case class Bind(varName: String, effect: Effect) extends Statement
case class Perform(effect: Effect) extends Statement
case class If(condition: Expr, thenBranch: Script) extends Statement
case class IfElse(condition: Expr, thenBranch: Script, elseBranch: Script) extends Statement
case class Loop(body: Script) extends Statement // TODO (explicit loops)
case class Return(value: Expr) extends Statement // TODO (early returns)

/* 
TODO: look into setting orderings to have these float to the top
ex (the first works as intended) (techincally both work as intended)

val player = entity("player")
  .onSpawn {
    setState("air", 100.0)
  }
  .onUpdate {
    when(getState("air") <= 0.0) {
      handleWith(noMove)
    }
    when(keyDown("d")) {
      move(500.0, 0.0)
    }
  }

vs

  .onUpdate {
    when(keyDown("d")) {
      move(500.0, 0.0)
    }
    when(getState("air") <= 0.0) {
      handleWith(noMove)
    }
  }

 With this, for scoped effects i believe it is just adding from the top to bottom, not necessarily in order of which has been true for the longest?
 also not fully sure if this causes issues
*/
case class HandleWith(handler: Handler, body: Script = Script(List.empty)) extends Statement

case object Noop extends Statement // TODO (empty branches)t

// Configurations for onSpawn (no coroutine yields)
case class Configure(config: SpawnConfig) extends Statement