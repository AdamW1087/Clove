package clove.compiler

import clove.ast.*
import clove.dsl.{Region, Behaviour}
import clove.dsl.worldbuilder.World

object WorldAnalyser:

  def analyse(world: World): WorldFeatures =

    // Collect every script in the world for effect scanning
    val entityScripts = world.entities.flatMap(e => List(e.spawnScript, e.updateScript))
    val triggerScripts = world.regions.flatMap {
      case Region(_, _, _, _, _, Behaviour.Trigger(onEnter, onExit), _, _) => List(onEnter, onExit)
      case _ => Nil
    }
    val customEffectScripts = world.customEffects.map { e =>
      e.impl(Expr.Var("resolved"), Expr.Var("dt"))
    }
    val allScripts = entityScripts ++ triggerScripts ++ customEffectScripts

    // Recursively collect all effects from a script
    def collectEffects(script: Script): List[Effect] =
      script.statements.flatMap {
        case Perform(e)                        => List(e)
        case Bind(_, e)                        => List(e)
        case If(_, t)                          => collectEffects(t)
        case IfElse(_, t, e)                   => collectEffects(t) ++ collectEffects(e)
        case Loop(body)                        => collectEffects(body)
        case HandleWith(_, body)               => collectEffects(body)
        case _                                 => Nil
      }

    // Check whether any script contains a HandleWith statement
    def hasHandleWith(script: Script): Boolean =
      script.statements.exists {
        case HandleWith(_, body) => true || hasHandleWith(body)
        case If(_, t)            => hasHandleWith(t)
        case IfElse(_, t, e)     => hasHandleWith(t) || hasHandleWith(e)
        case Loop(body)          => hasHandleWith(body)
        case _                   => false
      }

    val allEffects = allScripts.flatMap(collectEffects)

    def uses(p: Effect => Boolean): Boolean = allEffects.exists(p)

    val hasHandleWithInScripts = world.entities.exists { e =>
      hasHandleWith(e.updateScript) || hasHandleWith(e.spawnScript)
    }
    val hasBasicRegionHandlers = world.regions.exists {
      case Region(_, _, _, _, _, Behaviour.Basic(handlers), _, _) =>
        handlers.exists(h => h.handles.nonEmpty || h.impls.nonEmpty)
      case _ => false
    }

    WorldFeatures(
      usesGravity    = uses { case _: Effect.Gravity  => true; case _ => false },
      usesJump       = uses { case _: Effect.Jump     => true; case _ => false },
      usesMove       = uses { case _: Effect.Move     => true; case _ => false },
      usesCollides   = uses { case _: Effect.Collides => true; case _ => false },
      usesCamera     = uses { case _: Effect.Camera   => true; case _ => false },
      usesShowState  = uses { case _: Effect.ShowState => true; case _ => false },
      usesGlobals    = world.initialGlobals.nonEmpty ||
                       uses { case _: Effect.GetGlobal => true; case _: Effect.SetGlobal => true; case _ => false },
      usesTriggers   = world.regions.exists { case Region(_, _, _, _, _, _: Behaviour.Trigger, _, _) => true; case _ => false },
      usesAnimations = world.entities.exists(e =>
        e.spawnScript.statements.exists {
          case Configure(SpawnConfig.SetSpritesheet(_, _, _)) => true
          case _ => false
        }
      ),
      usesHandlers   = hasHandleWithInScripts || hasBasicRegionHandlers
    )