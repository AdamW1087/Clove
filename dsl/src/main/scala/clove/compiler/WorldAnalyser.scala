package clove.compiler

import clove.ast.*
import clove.dsl.{Region, Behaviour, World}

object WorldAnalyser:

  def analyse(world: World): WorldFeatures =

    // Collect every script in the world for effect scanning
    val entityScripts = world.entities.flatMap(e => List(e.spawnScript, e.initScript, e.updateScript))

    val customEffectScripts = world.customEffects.map { e =>
      e.impl(Expr.Var("resolved"), Expr.Var("dt"))
    }

    // Handler impl bodies
    def implBodies(handlers: Iterable[Handler]): List[Script] =
      handlers.flatMap(_.impls.values).map(_.body).toList

    def handlersInScript(script: Script): List[Handler] =
      script.statements.flatMap {
        case HandleWith(h, body) => h :: handlersInScript(body)
        case If(_, t)            => handlersInScript(t)
        case IfElse(_, t, e)     => handlersInScript(t) ++ handlersInScript(e)
        case _                   => Nil
      }

    val allHandlers =
      world.defaultHandlers ++
      world.regions.collect {
        case Region(_, _, _, _, _, Behaviour.Basic(hs), _, _, _) => hs
      }.flatten ++
      entityScripts.flatMap(handlersInScript)

    val handlerImplScripts = implBodies(allHandlers)

    val allScripts = entityScripts ++ customEffectScripts ++ handlerImplScripts

    // Recursively collect all effects from a script
    def collectEffects(script: Script): List[Effect[?]] =
      script.statements.flatMap {
        case Perform(e)                        => List(e)
        case Bind(_, e)                        => List(e)
        case Discard(e)                        => List(e)
        case If(_, t)                          => collectEffects(t)
        case IfElse(_, t, e)                   => collectEffects(t) ++ collectEffects(e)
        case HandleWith(_, body)               => collectEffects(body)
        case _                                 => Nil
      }

    // Check whether any script contains a HandleWith statement
    def hasHandleWith(script: Script): Boolean =
      script.statements.exists {
        case HandleWith(_, body) => true
        case If(_, t)            => hasHandleWith(t)
        case IfElse(_, t, e)     => hasHandleWith(t) || hasHandleWith(e)
        case _                   => false
      }

    val allEffects = allScripts.flatMap(collectEffects)

    def uses(p: Effect[?] => Boolean): Boolean = allEffects.exists(p)

    def scanExpr(p: Expr => Boolean)(expr: Expr): Boolean =
      p(expr) || (expr match
        case Expr.BinOp(_, l, r) => scanExpr(p)(l) || scanExpr(p)(r)
        case Expr.Not(e)         => scanExpr(p)(e)
        case _                   => false)

    def scanScript(p: Expr => Boolean)(script: Script): Boolean =
      script.statements.exists {
        case If(cond, t)         => scanExpr(p)(cond) || scanScript(p)(t)
        case IfElse(cond, t, e) => scanExpr(p)(cond) || scanScript(p)(t) || scanScript(p)(e)
        case HandleWith(_, body) => scanScript(p)(body)
        case _                   => false
      }

    val isEntityRead: Expr => Boolean =
      case Expr.EntityRead(_, _) => true
      case Expr.EntityExists(_)  => true
      case _                     => false

    val isJustPressed: Expr => Boolean =
      case Expr.JustPressed(_) => true
      case _                   => false

    def anyScript(p: Expr => Boolean): Boolean =
      world.entities.exists(e =>
        scanScript(p)(e.updateScript) || scanScript(p)(e.spawnScript) || scanScript(p)(e.initScript)
      )

    val usesEntityReadsVal = anyScript(isEntityRead)

    val hasHandleWithInScripts = world.entities.exists { e =>
      hasHandleWith(e.updateScript) || hasHandleWith(e.spawnScript) || hasHandleWith(e.initScript)
    }

    val hasBasicRegionHandlers = world.regions.exists {
      case Region(_, _, _, _, _, Behaviour.Basic(handlers), _, _, _) =>
        handlers.exists(h => h.handles.nonEmpty || h.impls.nonEmpty)
      case _ => false
    }

    WorldFeatures(
      usesGravity    = uses { case _: Effect.Gravity  => true; case _ => false },
      usesJump       = uses { case _: Effect.Jump     => true; case _ => false },
      usesMove       = uses { case _: Effect.Move     => true; case _ => false },
      usesCollides   = uses { case _: Effect.Collides => true; case _ => false },
      usesCamera     = uses { case _: Effect.Camera => true; case _: Effect.SetCamera => true; case _ => false },
      usesGlobals    = world.initialGlobals.nonEmpty ||
                       uses { case _: Effect.GetGlobal => true; case _: Effect.SetGlobal => true; case _ => false },
      usesAnimations = world.entities.exists(e =>
        e.spawnScript.statements.exists {
          case Configure(SpawnConfig.SetSpritesheet(_, _, _)) => true
          case _ => false
        }
      ),
      usesHandlers = hasHandleWithInScripts || hasBasicRegionHandlers,
      usesVisuals = world.regions.exists(_.visual.isDefined),
      usesEntityReads = usesEntityReadsVal,
      usesCrossEntityReads  = uses { case _: Effect.GetStateOf => true; case _ => false },
      usesCrossEntityWrites = uses { case _: Effect.SetStateOf => true; case _ => false },
      usesSpawnAt           = uses { case _: Effect.SpawnAt => true; case _ => false } || world.templates.nonEmpty,
      usesUI                = uses { case _: UI => true; case _ => false },
      usesUIBar             = uses { case _: UI.Bar     => true; case _ => false },
      usesUILabel           = uses { case _: UI.Label   => true; case _ => false },
      usesUISprites         = uses { case _: UI.Sprites => true; case _ => false },
      usesUISlots           = uses { case _: UI.Slots   => true; case _ => false },
      usesUIImage           = uses { case _: UI.Image   => true; case _ => false },
      usesJustPressed       = anyScript(isJustPressed),
      usesSound             = uses { case _: Effect.PlaySound => true; case _ => false },
      usesMusic             = uses { case _: Effect.Music => true; case _ => false },
    )