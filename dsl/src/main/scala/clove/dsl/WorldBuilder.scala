package clove.dsl

import clove.ast.*
import scala.collection.mutable.ListBuffer

case class World(
  setup: Script, 
  regions: List[Region], 
  entities: List[Entity],
  defaultHandlers: List[Handler] = List.empty
)

class WorldBuilder extends ScriptBuilder:
  val regions = ListBuffer[Region]()
  val entities = ListBuffer[Entity]()
  val defaultHandlers = ListBuffer[Handler]()

  def addRegion(r: Region): Unit = regions += r
  def addEntity(e: Entity): Unit = entities += e
  def addHandler(hs: List[Handler]): Unit = defaultHandlers ++= hs

def world(body: WorldBuilder ?=> Unit): World =
  val builder = WorldBuilder()
  body(using builder)
  World(
    builder.build(), 
    builder.regions.toList, 
    builder.entities.toList,
    builder.defaultHandlers.toList
  )