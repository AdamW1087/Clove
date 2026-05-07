package clove.ast

// Defines all logic in Clove
case class Script(statements: List[Statement])

sealed trait Statement

// Assigns a variable (varName) to the return value of an Effect
case class Bind(varName: String, effect: Effect) extends Statement

// Registers the effect as acting on an entity 
case class Perform(effect: Effect) extends Statement


case class If(condition: Expr, thenBranch: Script) extends Statement
case class IfElse(condition: Expr, thenBranch: Script, elseBranch: Script) extends Statement

// TODO
case class Loop(body: Script) extends Statement // explicit loops
case class Return(value: Expr) extends Statement // early returns
case object Noop extends Statement // empty branches

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
// Assigns a handler for a specified scope in an entity
case class HandleWith(handler: Handler, body: Script = Script(List.empty)) extends Statement


// Configurations for onSpawn (no coroutine yields)
case class Configure(config: SpawnConfig) extends Statement