package clove.dsl

import clove.ast.*

// Entities are what the Effects are applied on/ the movable elements of the world
case class Entity(
  name: String,
  spawnScript: Script = Script(List.empty),
  initScript: Script = Script(List.empty),
  updateScript: Script = Script(List.empty),
  tags: List[String] = List.empty
):
  // Spawn script for entities
  def onSpawn(body: ScriptBuilder ?=> Unit): Entity =
    copy(spawnScript = script(body))

  // Coroutine used when an entity actually spawns (not the start of the world)
  def onInit(body: ScriptBuilder ?=> Unit): Entity =
    copy(initScript = script(body))

  // Script that the entity will run every frame
  def onUpdate(body: ScriptBuilder ?=> Unit): Entity =
    copy(updateScript = script(body))

  def withTags(ts: String*): Entity =
    copy(tags = ts.toList)

// Creates a new entity
def entity(name: String): Entity = Entity(name)

given Conversion[Entity, Expr] = e => Expr.Var(e.name)
