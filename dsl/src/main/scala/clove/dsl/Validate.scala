package clove.dsl


import clove.dsl.*
import clove.ast.*

// Get effect name for nicer error messages
private def effectName(e: Effect[?]): String =
  e.getClass.getSimpleName.stripSuffix("$")

// Script trraversal helpers
private def collectConfigureInUpdate(script: Script): List[String] =
  script.statements.flatMap {
    case Configure(c)        => List(c.getClass.getSimpleName.stripSuffix("$"))
    case If(_, t)            => collectConfigureInUpdate(t)
    case IfElse(_, t, e)     => collectConfigureInUpdate(t) ++ collectConfigureInUpdate(e)
    case HandleWith(_, body) => collectConfigureInUpdate(body)
    case _                   => Nil
  }

private def collectSpritePaths(script: Script): List[String] =
  script.statements.flatMap {
    case Configure(SpawnConfig.SetSprite(path))            => List(path)
    case Configure(SpawnConfig.SetSpritesheet(path, _, _)) => List(path)
    case If(_, t)                                          => collectSpritePaths(t)
    case IfElse(_, t, e)                                   => collectSpritePaths(t) ++ collectSpritePaths(e)
    case HandleWith(_, body)                               => collectSpritePaths(body)
    case _                                                 => Nil
  }

private def collectQueryNames(script: Script): List[String] =
  script.statements.flatMap {
    case Bind(_, Effect.UserEffect(name)) => List(name.toLowerCase)
    case If(_, t)                    => collectQueryNames(t)
    case IfElse(_, t, e)             => collectQueryNames(t) ++ collectQueryNames(e)
    case HandleWith(_, body)         => collectQueryNames(body)
    case _                           => Nil
  }

private def collectCustomPerforms(script: Script): List[String] =
  script.statements.flatMap {
    case Perform(Effect.UserEffect(name)) => List(name.toLowerCase)
    case If(_, t)                        => collectCustomPerforms(t)
    case IfElse(_, t, e)                 => collectCustomPerforms(t) ++ collectCustomPerforms(e)
    case HandleWith(_, body)             => collectCustomPerforms(body)
    case _                               => Nil
  }

private def collectHandlers(script: Script): List[Handler] =
  script.statements.flatMap {
    case HandleWith(h, body) => h :: collectHandlers(body)
    case If(_, t)            => collectHandlers(t)
    case IfElse(_, t, e)     => collectHandlers(t) ++ collectHandlers(e)
    case _                   => Nil
  }

private def collectIllegalTriggerEffects(script: Script): List[String] =
  script.statements.flatMap {
    case Perform(Effect.SetState(_, _))  => Nil
    case Perform(Effect.SetGlobal(_, _)) => Nil
    case Perform(Effect.SetSize(_, _))   => Nil
    case Bind(_, Effect.GetState(_))     => Nil
    case Bind(_, Effect.GetGlobal(_))    => Nil
    case Perform(_: UI)                  => List("UI effects cannot be used in trigger scripts")
    case If(_, t)                        => collectIllegalTriggerEffects(t)
    case IfElse(_, t, e)                 => collectIllegalTriggerEffects(t) ++ collectIllegalTriggerEffects(e)
    case Perform(e)                      => List(effectName(e))
    case Bind(_, e)                      => List(effectName(e))
    case Discard(e)                      => List(effectName(e))
    case _                               => Nil
  }

private def collectObsGlobals(expr: Expr): List[String] = expr match
  case Expr.GlobalRead(key) => List(key)
  case Expr.BinOp(_, l, r)  => collectObsGlobals(l) ++ collectObsGlobals(r)
  case Expr.Not(e)          => collectObsGlobals(e)
  case _                    => Nil

private def collectStateDefs(script: Script): List[String] =
  script.statements.flatMap {
    case Perform(Effect.SetState(key, _)) => List(key)
    case If(_, t)                         => collectStateDefs(t)
    case IfElse(_, t, e)                  => collectStateDefs(t) ++ collectStateDefs(e)
    case HandleWith(_, body)              => collectStateDefs(body)
    case _                                => Nil
  }

private def collectStateReads(script: Script): List[String] =
  script.statements.flatMap {
    case Bind(_, Effect.GetState(key)) => List(key)
    case If(_, t)                      => collectStateReads(t)
    case IfElse(_, t, e)               => collectStateReads(t) ++ collectStateReads(e)
    case HandleWith(_, body)           => collectStateReads(body)
    case _                             => Nil
  }

private def animRuleBeforeSheet(script: Script): Boolean =
  val statements = script.statements
  val sheetIdx = statements.indexWhere {
    case Configure(SpawnConfig.SetSpritesheet(_, _, _)) => true
    case _ => false
  }
  val firstRuleIdx = statements.indexWhere {
    case Configure(SpawnConfig.AnimRule(_, _, _, _, _)) => true
    case _ => false
  }
  firstRuleIdx >= 0 && (sheetIdx < 0 || firstRuleIdx < sheetIdx)

private def collectCustomEffectStateKeys(effect: CustomEffect): Set[String] =
  def fromScript(script: Script): List[String] =
    script.statements.flatMap {
      case Bind(_, Effect.GetState(key))    => List(key)
      case Perform(Effect.SetState(key, _)) => List(key)
      case If(_, t)                         => fromScript(t)
      case IfElse(_, t, e)                  => fromScript(t) ++ fromScript(e)
      case _                                => Nil
    }
  fromScript(effect.impl(Expr.Var("resolved"), Expr.Var("dt"))).toSet

// Validation
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
      case _ => false
    }

  val missingSize = builder.entities
    .filterNot(e => hasSetSize(e.spawnScript) || hasSetSize(e.updateScript))
    .map(_.name)
  require(missingSize.isEmpty,
    s"Entities missing setSize: ${missingSize.mkString(", ")}")

  val missingTemplateSize = builder.templates.values
    .filterNot(e => hasSetSize(e.spawnScript) || hasSetSize(e.updateScript))
    .map(_.name).toList
  require(missingTemplateSize.isEmpty,
    s"Templates missing setSize: ${missingTemplateSize.mkString(", ")}")

  // Configure must not appear in update scripts
  val configInUpdate = builder.entities.flatMap(e => collectConfigureInUpdate(e.updateScript))
  require(configInUpdate.isEmpty,
    s"Spawn configuration must be in onSpawn, not onUpdate: ${configInUpdate.mkString(", ")}")

  // Sprite/spritesheet paths must exist on disk
  val missingSprites = builder.entities
    .flatMap(e => collectSpritePaths(e.spawnScript))
    .filterNot(path => os.exists(os.pwd / "src" / "main" / "resources" / os.RelPath(path)))
  require(missingSprites.isEmpty,
    s"Sprite files not found: ${missingSprites.mkString(", ")}")

  def collectUIImagePaths(script: Script): List[String] =
    script.statements.flatMap {
      case Perform(UI.Sprites(_, _, image, _, _, _, _)) => List(image)
      case Perform(UI.Slots(_, _, _, images, _, _))     => images
      case Perform(UI.Image(_, _, _, _, image, _))      => List(image)
      case If(_, t)                                     => collectUIImagePaths(t)
      case IfElse(_, t, e)                              => collectUIImagePaths(t) ++ collectUIImagePaths(e)
      case HandleWith(_, body)                          => collectUIImagePaths(body)
      case _                                            => Nil
    }

  val missingUIImages = builder.entities
    .flatMap(e => collectUIImagePaths(e.updateScript))
    .distinct
    .filterNot(path => os.exists(os.pwd / "src" / "main" / "resources" / os.RelPath(path)))
  require(missingUIImages.isEmpty,
    s"UI image files not found: ${missingUIImages.mkString(", ")}")

  // Region visual image paths must exist on disk
  val missingVisuals = builder.regions
    .flatMap(_.visual)
    .map(_.path)
    .distinct
    .filterNot(path => os.exists(os.pwd / "src" / "main" / "resources" / os.RelPath(path)))
  require(missingVisuals.isEmpty,
    s"Region visual image files not found: ${missingVisuals.mkString(", ")}")

  // AnimRule must appear after SetSpritesheet
  val animWithoutSheet = builder.entities.filter(e => animRuleBeforeSheet(e.spawnScript)).map(_.name)
  require(animWithoutSheet.isEmpty,
    s"animRule used before setSpritesheet in: ${animWithoutSheet.mkString(", ")}")

  // Regions must have positive dimensions
  val invalidRegions = builder.regions.filter(r => r.w <= 0 || r.h <= 0)
  require(invalidRegions.isEmpty, "Regions must have positive dimensions")

  // No duplicate region ids
  val regionIds = builder.regions.flatMap(_.id)
  val duplicateRegions = regionIds.groupBy(identity).filter(_._2.size > 1).keys
  require(duplicateRegions.isEmpty,
    s"Duplicate region ids: ${duplicateRegions.mkString(", ")}")

  // Trigger scripts may only use a restricted set of effects
  val illegalTriggerEffects = builder.regions.flatMap {
    case Region(id, _, _, _, _, Behaviour.Trigger(onEnter, onExit), _, _, _) =>
      val bad = collectIllegalTriggerEffects(onEnter) ++ collectIllegalTriggerEffects(onExit)
      bad.map(e => s"${id.getOrElse("unnamed trigger")}: $e")
    case _ => Nil
  }
  require(illegalTriggerEffects.isEmpty,
    s"Trigger scripts may only use setState/getState/setGlobal/getGlobal: ${illegalTriggerEffects.mkString(", ")}")

  // obsGlobal keys must be declared in the world globals
  val globalKeys = builder.globals.keySet
  val missingGlobals = builder.regions.flatMap {
    case Region(_, _, _, _, _, _, Some(cond), _, _) => collectObsGlobals(cond)
    case _ => Nil
  }.filterNot(globalKeys.contains)
  require(missingGlobals.isEmpty,
    s"obsGlobal keys missing from world globals: ${missingGlobals.mkString(", ")}")

  // Query effects must have a default handler value
  val queryNames = builder.entities
    .flatMap(e => collectQueryNames(e.updateScript) ++ collectQueryNames(e.spawnScript))
    .toSet

  val defaultHandlerKeys = builder.defaultHandlers.flatMap(_.handles.keys).map(_.toLowerCase).toSet

  val missingQueryDefaults = queryNames.filterNot(defaultHandlerKeys.contains)
  require(missingQueryDefaults.isEmpty,
    s"Query effects missing default handler values: ${missingQueryDefaults.mkString(", ")}")

  // Custom effect validation
  val registeredNames = builder.customEffects.map(_.name).toSet
  val builtInKeys = Set("move", "jump", "gravity", "despawn", "draw",
                        "setstate", "getstate", "setglobal", "getglobal", "collides", "camera", "setsize")
  val knownKeys = registeredNames ++ builtInKeys ++ queryNames

  val effectNames = builder.customEffects.map(_.name.toLowerCase)
  val duplicates = effectNames.groupBy(identity).filter(_._2.size > 1).keys
  require(duplicates.isEmpty,
    s"Duplicate custom effect names: ${duplicates.mkString(", ")}")

  val missingDefaults = registeredNames.filterNot(defaultHandlerKeys.contains)
  require(missingDefaults.isEmpty,
    s"Custom effects missing default handler values: ${missingDefaults.mkString(", ")}")

  val performedEffects = builder.entities
    .flatMap(e => collectCustomPerforms(e.updateScript) ++ collectCustomPerforms(e.spawnScript))
    .toSet
  val unregistered = performedEffects.filterNot(registeredNames.contains)
  require(unregistered.isEmpty,
    s"Performed custom effects not registered: ${unregistered.mkString(", ")}")

  // All handler keys must match a known effect
  val allHandlerKeys = (
    builder.defaultHandlers ++
    builder.regions.flatMap {
      case Region(_, _, _, _, _, Behaviour.Basic(handlers), _, _, _) => handlers
      case _ => Nil
    } ++
    builder.entities.flatMap(e => collectHandlers(e.updateScript) ++ collectHandlers(e.spawnScript))
  ).flatMap(h => h.handles.keys ++ h.impls.keys).map(_.toLowerCase).toSet

  val invalidKeys = allHandlerKeys.filterNot(knownKeys.contains)
  require(invalidKeys.isEmpty,
    s"Handler keys don't match any known effect: ${invalidKeys.mkString(", ")}")

  // States read in onUpdate must be defined in onSpawn
  val undefinedStateReads = builder.entities.flatMap { e =>
    val defined = collectStateDefs(e.spawnScript).toSet
    val read = collectStateReads(e.updateScript).toSet ++ collectStateReads(e.initScript).toSet
    (read -- defined).map(key => s"${e.name}: $key")
  }
  require(undefinedStateReads.isEmpty,
    s"States read in onUpdate/onInit but not defined in onSpawn: ${undefinedStateReads.mkString(", ")}")

  // States used by custom effects must be defined in spawn of any entity that performs them
  val customEffectKeys = builder.customEffects.map(e => e.name -> collectCustomEffectStateKeys(e)).toMap

  val undefinedCustomStateKeys = builder.entities.flatMap { entity =>
    val defined = collectStateDefs(entity.spawnScript).toSet
    val performed = collectCustomPerforms(entity.updateScript) ++ collectCustomPerforms(entity.spawnScript)
    performed.flatMap { name =>
      customEffectKeys.get(name).toList.flatMap { keys =>
        (keys -- defined).map(key => s"${entity.name} performs '$name' but '$key' not defined in onSpawn")
      }
    }
  }
  require(undefinedCustomStateKeys.isEmpty,
    s"Custom effect state keys not defined in onSpawn: ${undefinedCustomStateKeys.mkString(", ")}")