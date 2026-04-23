package clove.dsl

import clove.ast.*
import scala.collection.mutable.ListBuffer

case class World(
  setup: Script, 
  regions: List[Region], 
  entities: List[Entity],
  defaultHandlers: List[Handler] = List.empty,
  customEffects: List[Effect.Custom] = List.empty
)

class WorldBuilder extends ScriptBuilder:
  val regions = ListBuffer[Region]()
  val entities = ListBuffer[Entity]()
  val defaultHandlers = ListBuffer[Handler]()
  val customEffects = ListBuffer[Effect.Custom]()

  def addRegion(r: Region): Unit = regions += r
  def addEntity(e: Entity): Unit = entities += e
  def addHandler(hs: List[Handler]): Unit = defaultHandlers ++= hs
  def addCustomEffect(e: Effect.Custom): Unit = customEffects += e

/*

Need following checks:

Effects arent defined twice
Default handler is always put in
Custom effects are fully defined

Things being attached twice (warning?)
*/
def world(body: WorldBuilder ?=> Unit): World =
  val builder = WorldBuilder()
  body(using builder)
  World(
    builder.build(), 
    builder.regions.toList, 
    builder.entities.toList,
    builder.defaultHandlers.toList,
    builder.customEffects.toList
  )