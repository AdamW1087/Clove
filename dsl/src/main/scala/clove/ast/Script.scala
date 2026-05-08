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
update: i think having scoped effects staying where they are in terms of other effects is important
e.g. pushing a noMove handler before trying to move
the main question is is it safe to float things like scoped effects to the top? and performs at the bottom 

also
perform(Drown)
showState("air")

works as expeected

showState("air")
perform(Drown)

only shows air of previous frame (does make sense)
similarly

val drownFast = handler("drown" -> 10.0)


when(keyDown("w")) {
  handleWith(drownFast)
}

perform(Drown)

the above works with drowning fast

perform(Drown)

when(keyDown("w")) {
  handleWith(drownFast)
}

as perform is done before the handler is on, it is not acted on the player

should performs automatically float to the bottom?

*/
// Assigns a handler for a specified scope in an entity
case class HandleWith(handler: Handler, body: Script = Script(List.empty)) extends Statement


// Configurations for onSpawn (no coroutine yields)
case class Configure(config: SpawnConfig) extends Statement