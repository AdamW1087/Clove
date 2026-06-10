package clove.ast

// Defines all logic in Clove
case class Script(statements: List[Statement])

sealed trait Statement

// Assigns a variable (varName) to the return value of an Effect
case class Bind(varName: String, effect: Effect[Expr]) extends Statement

// Peforms Effects that do not return
case class Perform(effect: Effect[Unit]) extends Statement

// Continuation discarding
case class Discard(effect: Effect[Nothing]) extends Statement


case class If(condition: Expr, thenBranch: Script) extends Statement
case class IfElse(condition: Expr, thenBranch: Script, elseBranch: Script) extends Statement

// Assigns a handler for a specified scope in an entity
case class HandleWith(handler: Handler, body: Script = Script(List.empty)) extends Statement


// Configurations for onSpawn (no coroutine yields)
case class Configure(config: SpawnConfig) extends Statement