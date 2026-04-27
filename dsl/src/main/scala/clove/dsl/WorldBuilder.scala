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
  def addCustomEffects(es: List[Effect.Custom]): Unit = customEffects ++= es


def validate(builder: WorldBuilder): Unit =

  // World must have at least one entity
  require(builder.entities.nonEmpty, "World must have at least one entity")

  // World must have default handlers
  require(builder.defaultHandlers.nonEmpty, "World must have at least one default handler")


  // No two entities with the same name
  val entityNames = builder.entities.map(_.name)
  val duplicateEntities = entityNames.groupBy(identity).filter(_._2.size > 1).keys
  require(duplicateEntities.isEmpty,
    s"Duplicate entity names: ${duplicateEntities.mkString(", ")}")


  // Regions have valid dimensions
  val invalidRegions = builder.regions.filter(r => r.w <= 0 || r.h <= 0)
  require(invalidRegions.isEmpty,
    s"Regions must have positive dimensions")

  // Custom effects and handlers
  val registeredNames = builder.customEffects.map(_.name.toLowerCase).toSet
  val builtInKeys = Set("move", "jump", "gravity", "spawn", "despawn", "draw", "setstate", "getstate", "collides", "camera")
  val knownKeys = registeredNames ++ builtInKeys

  // No duplicate custom effect names
  val effectNames = builder.customEffects.map(_.name.toLowerCase)
  val duplicates = effectNames.groupBy(identity).filter(_._2.size > 1).keys
  require(duplicates.isEmpty,
    s"Duplicate custom effect names: ${duplicates.mkString(", ")}")


  // Every custom effect has a default handler value
  val defaultHandlerKeys = builder.defaultHandlers.flatMap(_.handles.keys).map(_.toLowerCase).toSet
  val missingDefaults = registeredNames.filterNot(defaultHandlerKeys.contains)
  require(missingDefaults.isEmpty,
    s"Custom effects missing default handler values: ${missingDefaults.mkString(", ")}")


  // Every performed custom effect is registered
  def collectCustomPerforms(script: Script): List[String] =
    script.statements.flatMap {
      case Perform(Effect.Custom(name, _))   => List(name.toLowerCase)
      case Bind(_, Effect.Custom(name, _))   => List(name.toLowerCase)
      case If(_, thenBranch)                 => collectCustomPerforms(thenBranch)
      case Loop(body)                        => collectCustomPerforms(body)
      case HandleWith(_, body)               => collectCustomPerforms(body)
      case IfElse(_, thenBranch, elseBranch) =>
        collectCustomPerforms(thenBranch) ++ collectCustomPerforms(elseBranch)
      case _                                 => Nil
    }

  val performedEffects = builder.entities
    .flatMap(e => collectCustomPerforms(e.updateScript) ++ collectCustomPerforms(e.spawnScript))
    .toSet
  val unregistered = performedEffects.filterNot(registeredNames.contains)
  require(unregistered.isEmpty,
    s"Performed custom effects not registered: ${unregistered.mkString(", ")}")


  // All handler keys across default handlers, regions, and entity scripts match a known effect
  def collectHandlers(script: Script): List[Handler] =
    script.statements.flatMap {
      case HandleWith(h, body)               => h :: collectHandlers(body)
      case If(_, thenBranch)                 => collectHandlers(thenBranch)
      case IfElse(_, thenBranch, elseBranch) => collectHandlers(thenBranch) ++ collectHandlers(elseBranch)
      case Loop(body)                        => collectHandlers(body)
      case _                                 => Nil
    }

  val allHandlerKeys = (
    builder.defaultHandlers ++
    builder.regions.flatMap(_.handlers) ++
    builder.entities.flatMap(e => collectHandlers(e.updateScript) ++ collectHandlers(e.spawnScript))
  ).flatMap(_.handles.keys).map(_.toLowerCase).toSet

  val invalidKeys = allHandlerKeys.filterNot(knownKeys.contains)
  require(invalidKeys.isEmpty,
    s"Handler keys don't match any known effect: ${invalidKeys.mkString(", ")}")

def world(body: WorldBuilder ?=> Unit): World =
  val builder = WorldBuilder()
  body(using builder)

  validate(builder)

  World(
    builder.build(), 
    builder.regions.toList, 
    builder.entities.toList,
    builder.defaultHandlers.toList,
    builder.customEffects.toList
  )