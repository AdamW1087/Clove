package clove.dsl.helpers

import clove.ast.*
import clove.dsl.{ScriptBuilder, script, ===}


// Control flow

class WhenClause(cond: Expr, thenBody: Script)(using b: ScriptBuilder):
  b += If(cond, thenBody)

  def otherwise(elseBody: ScriptBuilder ?=> Unit): Unit =
    b.replaceLast(IfElse(cond, thenBody, script(elseBody)))

def when(cond: Expr)(body: ScriptBuilder ?=> Unit)(using b: ScriptBuilder): WhenClause =
  WhenClause(cond, script(body))

// Finds the first matching behaviour for the current value of a state key
def switchState(stateKey: String, states: (String, ScriptBuilder ?=> Unit)*)(using b: ScriptBuilder): Unit =
  if states.isEmpty then return

  val current = getState(stateKey)

  def buildChain(remaining: Seq[(String, ScriptBuilder ?=> Unit)]): Unit =
    remaining match
      case Seq((_, behaviour)) =>
        behaviour
      case (name, behaviour) +: tail =>
        when(current === name) {
          behaviour
        } otherwise {
          buildChain(tail)
        }

  buildChain(states)
