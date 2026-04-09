package clove.dsl

import clove.ast.*

case class Entity(
  name: String,
  spawnScript: Script = Script(List.empty),
  updateScript: Script = Script(List.empty),
):
  def onSpawn(body: ScriptBuilder ?=> Unit): Entity =
    copy(spawnScript = script(body))

  def onUpdate(body: ScriptBuilder ?=> Unit): Entity =
    copy(updateScript = script(body))

def entity(name: String): Entity = Entity(name)

given Conversion[Entity, Expr] = e => Expr.Var(e.name)
