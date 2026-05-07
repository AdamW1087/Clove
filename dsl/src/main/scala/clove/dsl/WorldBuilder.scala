package clove.dsl

import clove.ast.*
import scala.collection.mutable.ListBuffer


case class World(
  setup: Script,
  regions: List[Region],
  entities: List[Entity],
  defaultHandlers: List[Handler] = List.empty,
  customEffects: List[Effect.Custom] = List.empty,
  initialGlobals: Map[String, Expr] = Map.empty
)

class WorldBuilder extends ScriptBuilder:
  val regions         = ListBuffer[Region]()
  val entities        = ListBuffer[Entity]()
  val defaultHandlers = ListBuffer[Handler]()
  val customEffects   = ListBuffer[Effect.Custom]()
  val globals         = scala.collection.mutable.Map[String, Expr]()

  def addRegion(r: Region): Unit                      = regions += r
  def addEntity(e: Entity): Unit                      = entities += e
  def addHandler(hs: List[Handler]): Unit             = defaultHandlers ++= hs
  def addCustomEffects(es: List[Effect.Custom]): Unit = customEffects ++= es
  def setGlobal(key: String, value: Expr): Unit       = globals(key) = value


/*
to add:

any states used in update are defined in spawn 
any states used by custom effects need be defined on player? all entities??
 */
def validate(builder: WorldBuilder): Unit =

  require(builder.entities.nonEmpty, "World must have at least one entity")
  require(builder.defaultHandlers.nonEmpty, "World must have at least one default handler")

  // No duplicate entity names
  val entityNames = builder.entities.map(_.name)
  val duplicateEntities = entityNames.groupBy(identity).filter(_._2.size > 1).keys
  require(duplicateEntities.isEmpty,
    s"Duplicate entity names: ${duplicateEntities.mkString(", ")}")

  // Every entity must have setSize defined
  def hasSetSize(script: Script): Boolean =
    script.statements.exists {
      case Perform(Effect.SetSize(_, _)) => true
      case Bind(_, Effect.SetSize(_, _)) => true
      case _ => false
    }

  val missingSize = builder.entities
    .filterNot(e => hasSetSize(e.spawnScript) || hasSetSize(e.updateScript))
    .map(_.name)
  require(missingSize.isEmpty,
    s"Entities missing setSize: ${missingSize.mkString(", ")}")

  // Configure must not appear in update scripts
  def collectConfigureInUpdate(script: Script): List[String] =
    script.statements.flatMap {
      case Configure(c)                          => List(c.getClass.getSimpleName)
      case If(_, thenBranch)                     => collectConfigureInUpdate(thenBranch)
      case IfElse(_, thenBranch, elseBranch)     => collectConfigureInUpdate(thenBranch) ++ collectConfigureInUpdate(elseBranch)
      case Loop(body)                            => collectConfigureInUpdate(body)
      case HandleWith(_, body)                   => collectConfigureInUpdate(body)
      case _                                     => Nil
    }

  val configInUpdate = builder.entities.flatMap(e => collectConfigureInUpdate(e.updateScript))
  require(configInUpdate.isEmpty,
    s"Spawn configuration must be in onSpawn, not onUpdate: ${configInUpdate.mkString(", ")}")

  // Sprite/spritesheet paths exist
  def collectSpritePaths(script: Script): List[String] =
    script.statements.flatMap {
      case Configure(SpawnConfig.SetSprite(path))            => List(path)
      case Configure(SpawnConfig.SetSpritesheet(path, _, _)) => List(path)
      case If(_, thenBranch)                                 => collectSpritePaths(thenBranch)
      case IfElse(_, thenBranch, elseBranch)                 => collectSpritePaths(thenBranch) ++ collectSpritePaths(elseBranch)
      case Loop(body)                                        => collectSpritePaths(body)
      case HandleWith(_, body)                               => collectSpritePaths(body)
      case _                                                 => Nil
    }

  val missingSprites = builder.entities
    .flatMap(e => collectSpritePaths(e.spawnScript))
    .filterNot(path => os.exists(os.pwd / "src" / "main" / "resources" / os.RelPath(path)))
  require(missingSprites.isEmpty,
    s"Sprite files not found: ${missingSprites.mkString(", ")}")

  // AnimRule must appear after SetSpritesheet
  def animRuleBeforeSheet(script: Script): Boolean =
    val statements = script.statements
    val sheetIdx = statements.indexWhere {
      case Configure(SpawnConfig.SetSpritesheet(_, _, _)) => true
      case _ => false
    }
    val firstRuleIdx = statements.indexWhere {
      case Configure(SpawnConfig.AnimRule(_, _, _, _)) => true
      case _ => false
    }
    firstRuleIdx >= 0 && (sheetIdx < 0 || firstRuleIdx < sheetIdx)
 
  val animWithoutSheet = builder.entities.filter(e => animRuleBeforeSheet(e.spawnScript)).map(_.name)
  require(animWithoutSheet.isEmpty,
    s"animRule used before setSpritesheet in: ${animWithoutSheet.mkString(", ")}")

  // Regions have valid dimensions
  val invalidRegions = builder.regions.filter(r => r.w <= 0 || r.h <= 0)
  require(invalidRegions.isEmpty, "Regions must have positive dimensions")

  // No duplicate region ids
  val regionIds = builder.regions.flatMap(_.id)
  val duplicateRegions = regionIds.groupBy(identity).filter(_._2.size > 1).keys
  require(duplicateRegions.isEmpty,
    s"Duplicate region ids: ${duplicateRegions.mkString(", ")}")

  // Trigger scripts must only use setState/getState/setGlobal/getGlobal
  def collectIllegalTriggerEffects(script: Script): List[String] =
    script.statements.flatMap {
      case Perform(Effect.SetState(_, _))     => Nil
      case Bind(_, Effect.GetState(_))        => Nil
      case Perform(Effect.SetGlobal(_, _))    => Nil
      case Bind(_, Effect.GetGlobal(_))       => Nil
      case Perform(Effect.SetSize(_, _))      => Nil
      case If(_, thenBranch)                  => collectIllegalTriggerEffects(thenBranch)
      case IfElse(_, thenBranch, elseBranch)  => collectIllegalTriggerEffects(thenBranch) ++ collectIllegalTriggerEffects(elseBranch)
      case Perform(e)                         => List(e.getClass.getSimpleName)
      case Bind(_, e)                         => List(e.getClass.getSimpleName)
      case _                                  => Nil
    }

  val illegalTriggerEffects = builder.regions.flatMap {
    case Region(id, _, _, _, _, Behaviour.Trigger(onEnter, onExit), _, _) =>
      val bad = collectIllegalTriggerEffects(onEnter) ++ collectIllegalTriggerEffects(onExit)
      bad.map(e => s"${id.getOrElse("unnamed trigger")}: $e")
    case _ => Nil
  }
  require(illegalTriggerEffects.isEmpty,
    s"Trigger scripts may only use setState/getState/setGlobal/getGlobal: ${illegalTriggerEffects.mkString(", ")}")

  // obsGlobal keys must exist in initialGlobals
  def collectObsGlobals(expr: Expr): List[String] = expr match
    case Expr.GlobalRead(key)    => List(key)
    case Expr.BinOp(_, l, r)     => collectObsGlobals(l) ++ collectObsGlobals(r)
    case Expr.Not(e)             => collectObsGlobals(e)
    case _                       => Nil

  val globalKeys = builder.globals.keySet
  val missingGlobals = builder.regions.flatMap {
    case Region(_, _, _, _, _, _, Some(cond), _) => collectObsGlobals(cond)
    case _ => Nil
  }.filterNot(globalKeys.contains)
  require(missingGlobals.isEmpty,
    s"obsGlobal keys missing from world globals: ${missingGlobals.mkString(", ")}")

  // Query effects must have a default handler value
  def collectQueryNames(script: Script): List[String] =
    script.statements.flatMap {
      case Perform(Effect.Query(name))        => List(name.toLowerCase)
      case Bind(_, Effect.Query(name))        => List(name.toLowerCase)
      case If(_, thenBranch)                  => collectQueryNames(thenBranch)
      case IfElse(_, thenBranch, elseBranch)  => collectQueryNames(thenBranch) ++ collectQueryNames(elseBranch)
      case Loop(body)                         => collectQueryNames(body)
      case HandleWith(_, body)                => collectQueryNames(body)
      case _                                  => Nil
    }

  val queryNames = builder.entities
    .flatMap(e => collectQueryNames(e.updateScript) ++ collectQueryNames(e.spawnScript))
    .toSet

  val defaultHandlerKeys = builder.defaultHandlers.flatMap(_.handles.keys).map(_.toLowerCase).toSet

  val missingQueryDefaults = queryNames.filterNot(defaultHandlerKeys.contains)
  require(missingQueryDefaults.isEmpty,
    s"Query effects missing default handler values: ${missingQueryDefaults.mkString(", ")}")

  // Custom effect validation
  val registeredNames = builder.customEffects.map(_.name.toLowerCase).toSet
  val builtInKeys = Set("move", "jump", "gravity", "spawn", "despawn", "draw",
                        "setstate", "getstate", "setglobal", "getglobal", "collides", "camera", "setsize")
  val knownKeys = registeredNames ++ builtInKeys ++ queryNames

  val effectNames = builder.customEffects.map(_.name.toLowerCase)
  val duplicates = effectNames.groupBy(identity).filter(_._2.size > 1).keys
  require(duplicates.isEmpty,
    s"Duplicate custom effect names: ${duplicates.mkString(", ")}")

  val missingDefaults = registeredNames.filterNot(defaultHandlerKeys.contains)
  require(missingDefaults.isEmpty,
    s"Custom effects missing default handler values: ${missingDefaults.mkString(", ")}")

  def collectCustomPerforms(script: Script): List[String] =
    script.statements.flatMap {
      case Perform(Effect.Custom(name, _))    => List(name.toLowerCase)
      case Bind(_, Effect.Custom(name, _))    => List(name.toLowerCase)
      case If(_, thenBranch)                  => collectCustomPerforms(thenBranch)
      case Loop(body)                         => collectCustomPerforms(body)
      case HandleWith(_, body)                => collectCustomPerforms(body)
      case IfElse(_, thenBranch, elseBranch)  => collectCustomPerforms(thenBranch) ++ collectCustomPerforms(elseBranch)
      case _                                  => Nil
    }

  val performedEffects = builder.entities
    .flatMap(e => collectCustomPerforms(e.updateScript) ++ collectCustomPerforms(e.spawnScript))
    .toSet
  val unregistered = performedEffects.filterNot(registeredNames.contains)
  require(unregistered.isEmpty,
    s"Performed custom effects not registered: ${unregistered.mkString(", ")}")

  def collectHandlers(script: Script): List[Handler] =
    script.statements.flatMap {
      case HandleWith(h, body)                => h :: collectHandlers(body)
      case If(_, thenBranch)                  => collectHandlers(thenBranch)
      case IfElse(_, thenBranch, elseBranch)  => collectHandlers(thenBranch) ++ collectHandlers(elseBranch)
      case Loop(body)                         => collectHandlers(body)
      case _                                  => Nil
    }

  val allHandlerKeys = (
    builder.defaultHandlers ++
    builder.regions.flatMap {
      case Region(_, _, _, _, _, Behaviour.Basic(handlers), _, _) => handlers
      case _ => Nil
    } ++
    builder.entities.flatMap(e => collectHandlers(e.updateScript) ++ collectHandlers(e.spawnScript))
  ).flatMap(h => h.handles.keys ++ h.impls.keys).map(_.toLowerCase).toSet

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
    builder.customEffects.toList,
    builder.globals.toMap
  )