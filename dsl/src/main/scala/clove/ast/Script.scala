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

// Note: statement order is deliberately preserved.
// Scoped effects (HandleWith) must stay where they are written relative to
// performs, e.g. pushing a noMove handler must happen before the move,
// so statements are never reordered or floated

// Assigns a handler for a specified scope in an entity
case class HandleWith(handler: Handler, body: Script = Script(List.empty)) extends Statement


// Configurations for onSpawn (no coroutine yields)
case class Configure(config: SpawnConfig) extends Statement


// Generic script traversal
object ScriptTraversal:

  def collectStatements[A](script: Script)(extract: Statement => List[A]): List[A] =
    script.statements.flatMap { stmt =>
      val here = extract(stmt)
      val nested = stmt match
        case If(_, t)            => collectStatements(t)(extract)
        case IfElse(_, t, e)     => collectStatements(t)(extract) ++ collectStatements(e)(extract)
        case HandleWith(_, body) => collectStatements(body)(extract)
        case _                   => Nil
      here ++ nested
    }

  def existsStatement(script: Script)(p: Statement => Boolean): Boolean =
    script.statements.exists { stmt =>
      p(stmt) || (stmt match
        case If(_, t)            => existsStatement(t)(p)
        case IfElse(_, t, e)     => existsStatement(t)(p) || existsStatement(e)(p)
        case HandleWith(_, body) => existsStatement(body)(p)
        case _                   => false)
    }