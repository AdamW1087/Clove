package clove.dsl

import clove.ast.*
import scala.collection.mutable.ListBuffer

case class World(setup: Script, regions: List[Region], entities: List[Entity])

class WorldBuilder extends ScriptBuilder:
  val regions = ListBuffer[Region]()
  val entities = ListBuffer[Entity]()

  def addRegion(r: Region): Unit = regions += r
  def addEntity(e: Entity): Unit = entities += e

def world(body: WorldBuilder ?=> Unit): World =
  val builder = WorldBuilder()
  body(using builder)
  World(builder.build(), builder.regions.toList, builder.entities.toList)