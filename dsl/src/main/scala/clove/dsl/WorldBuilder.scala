package clove.dsl

import clove.ast.*
import clove.dsl.*
import scala.collection.mutable.ListBuffer

// The entire world
case class World(
  setup: Script, // Main definition script
  regions: List[Region],
  entities: List[Entity],
  defaultHandlers: List[Handler] = List.empty, // The base constants handlers
  customEffects: List[Effect.Custom] = List.empty, // Any user made effects
  initialGlobals: Map[String, Expr] = Map.empty, // Global var definition
  templates: Map[String, Entity] = Map.empty     // Entity templates for spawnAt
)

// Builder for the world
class WorldBuilder extends ScriptBuilder:
  val regions         = ListBuffer[Region]()
  val entities        = ListBuffer[Entity]()
  val defaultHandlers = ListBuffer[Handler]()
  val customEffects   = ListBuffer[Effect.Custom]()
  val globals         = scala.collection.mutable.Map[String, Expr]()
  val templates       = scala.collection.mutable.Map[String, Entity]()

  def addRegion(r: Region): Unit                      = regions += r
  def addEntity(e: Entity): Unit                      = entities += e
  def addHandler(hs: List[Handler]): Unit             = defaultHandlers ++= hs
  def addCustomEffects(es: List[Effect.Custom]): Unit = customEffects ++= es
  def setGlobal(key: String, value: Expr): Unit       = globals(key) = value
  def addTemplate(e: Entity): Unit                    = templates(e.name) = e

// Builds and validates the world
def world(body: WorldBuilder ?=> Unit): World =
  val builder = WorldBuilder()
  body(using builder)

  validate(builder)

  World(
    builder.build(),
    builder.regions.toList,
    builder.entities.toList,
    builder.defaultHandlers.toList,
    builder.customEffects.toList,
    builder.globals.toMap,
    builder.templates.toMap
  )