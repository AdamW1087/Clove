package clove.dsl.helpers

import clove.ast.*
import clove.dsl.{Entity, ScriptBuilder, script}


// Perform effect

// Performs an Effect with no return
def perform(effect: Effect[Unit])(using b: ScriptBuilder): Unit =
  b += Perform(effect)

// Binds the return value of an effect that resumes with Expr
def bind(effect: Effect[Expr])(using b: ScriptBuilder): Expr =
  val varName = b.nextVar()
  b += Bind(varName, effect)
  Expr.Var(varName)

// Performs an effect that resumes with a value
def perform(effect: Effect[Expr])(using b: ScriptBuilder): Expr =
  bind(effect)

def perform(effect: CustomEffect[Unit])(using b: ScriptBuilder): Unit =
  perform(effect.toEffect)

def perform(effect: CustomEffect[Expr])(using b: ScriptBuilder): Expr =
  perform(effect.toEffect)


// Physics and entity effects

def move(dx: Double, dy: Double)(using b: ScriptBuilder): Expr =
  perform(Effect.Move(Expr.Num(dx), Expr.Num(dy)))

def playSound(path: String)(using b: ScriptBuilder): Unit =
  perform(Effect.PlaySound(path))

def music()(using b: ScriptBuilder): Unit =
  perform(Effect.Music())

def despawn()(using b: ScriptBuilder): Unit =
  b += Discard(Effect.Despawn())

def setSize(width: Double, height: Double)(using b: ScriptBuilder): Unit =
  perform(Effect.SetSize(width, height))

def collides(target: Expr)(using b: ScriptBuilder): Expr =
  bind(Effect.Collides(target))


// State management

def setState(key: String, value: Expr)(using b: ScriptBuilder): Unit =
  perform(Effect.SetState(key, value))

def getState(key: String)(using b: ScriptBuilder): Expr =
  bind(Effect.GetState(key))

def setGlobal(key: String, value: Expr)(using b: ScriptBuilder): Unit =
  perform(Effect.SetGlobal(key, value))

def getGlobal(key: String)(using b: ScriptBuilder): Expr =
  bind(Effect.GetGlobal(key))

def setStateOf(targetId: String, key: String, value: Expr)(using b: ScriptBuilder): Unit =
  perform(Effect.SetStateOf(targetId, key, value))

def getStateOf(targetId: String, key: String)(using b: ScriptBuilder): Expr =
  bind(Effect.GetStateOf(targetId, key))


// Queries and custom effects

def queryKey(name: String): QueryKey = QueryKey(name)

// Custom effect whose impl resumes Unit
def customEffect(name: String)(impl: (Expr, Expr) => Script): CustomEffect[Unit] =
  CustomEffect[Unit](name, impl)

// Custom effect whose impl resumes with a value via resumeWith
def customValueEffect(name: String)(impl: (Expr, Expr) => Script): CustomEffect[Expr] =
  CustomEffect[Expr](name, impl)


// Resumption (inside handler impls)

def resumeWrite()(using b: ScriptBuilder): Unit =
  perform(Effect.ResumeWrite())

def resumeRead()(using b: ScriptBuilder): Unit =
  perform(Effect.ResumeRead())

// Resume the continuation with a value
def resumeWith(value: Expr)(using b: ScriptBuilder): Unit =
  perform(Effect.ResumeWith(value))


// Handler scoping (script-level)

def propagate(): Expr = Expr.Propagate

// Note: handleWith must be declared before perform(effect) to have the handler in the stack
def handleWith(handler: Handler)(using b: ScriptBuilder): Unit =
  b += HandleWith(handler)

def handleWith(handler: Handler)(body: ScriptBuilder ?=> Unit)(using b: ScriptBuilder): Unit =
  b += HandleWith(handler, script(body))


// Camera and spawning (script-level)

// Follow the entity running this script
def setCamera(zoom: Double = 1.0, deadzoneX: Double = 0.0, deadzoneY: Double = 0.0)(using b: ScriptBuilder): Unit =
  perform(Effect.Camera(zoom, deadzoneX, deadzoneY))

// Follow a named entity
def setCameraE(entity: Entity, zoom: Double = 1.0, deadzoneX: Double = 0.0, deadzoneY: Double = 0.0)(using b: ScriptBuilder): Unit =
  perform(Effect.SetCamera(entity.name, zoom, deadzoneX, deadzoneY))

// Spawn a registered template entity at the given position
def spawnAt(templateName: String, x: Expr, y: Expr)(using b: ScriptBuilder): Unit =
  perform(Effect.SpawnAt(templateName, x, y))