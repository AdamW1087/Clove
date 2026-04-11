package clove.dsl

import clove.ast.*
import scala.collection.mutable.ListBuffer

case class World(
  setup: Script,
  regions: List[Region],
  entities: List[Entity],
  camera: Option[CameraConfig] = None
)

class WorldBuilder extends ScriptBuilder:
  val regions = ListBuffer[Region]()
  val entities = ListBuffer[Entity]()
  var camera: Option[CameraConfig] = None

  def addRegion(r: Region): Unit = regions += r
  def addEntity(e: Entity): Unit = entities += e
  def setCamera(c: CameraConfig): Unit = camera = Some(c)

def world(body: WorldBuilder ?=> Unit): World =
  val builder = WorldBuilder()
  body(using builder)
  World(builder.build(), builder.regions.toList, builder.entities.toList, builder.camera)