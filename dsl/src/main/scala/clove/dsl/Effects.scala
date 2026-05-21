package clove.dsl
import clove.dsl.WorldBuilder

import clove.ast.*

// Performs an effect, discarding the return value
def perform(effect: Effect[Unit])(using b: ScriptBuilder): Unit =
  b += Perform(effect)

// Perform wrapper for custom effects
def perform(effect: CustomEffect)(using b: ScriptBuilder): Unit =
  perform(effect.toEffect)

// Binds the return value of an effect that resumes with Expr
def bind(effect: Effect[Expr])(using b: ScriptBuilder): Expr =
  val varName = b.nextVar()
  b += Bind(varName, effect)
  Expr.Var(varName)


// Physics effects
def move(dx: Double, dy: Double)(using b: ScriptBuilder): Unit =
  perform(Effect.Move(Expr.Num(dx), Expr.Num(dy)))

def jump()(using b: ScriptBuilder): Unit =
  perform(Effect.Jump())

def despawn()(using b: ScriptBuilder): Unit =
  b += Discard(Effect.Despawn())

def setSize(width: Double, height: Double)(using b: ScriptBuilder): Unit =
  perform(Effect.SetSize(width, height))


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

// Define a global variable in world block
def global(key: String, value: Expr)(using b: WorldBuilder): Unit =
  b.setGlobal(key, value)


// Queries and custom effects
def queryKey(name: String): QueryKey = QueryKey(name)

def collides(target: Expr)(using b: ScriptBuilder): Expr =
  bind(Effect.Collides(target))

// CustomEffect is both an Effect (for perform) and an EffectKey (for handlers)
def customEffect(name: String)(impl: (Expr, Expr) => Script): CustomEffect =
  CustomEffect(name, impl)

def register(effects: CustomEffect*)(using b: WorldBuilder): Unit =
  b.addCustomEffects(effects.toList)


// Control flow
class WhenClause(cond: Expr, thenBody: Script)(using b: ScriptBuilder):
  b += If(cond, thenBody)

  def otherwise(elseBody: ScriptBuilder ?=> Unit): Unit =
    b.replaceLast(IfElse(cond, thenBody, script(elseBody)))

def when(cond: Expr)(body: ScriptBuilder ?=> Unit)(using b: ScriptBuilder): WhenClause =
  WhenClause(cond, script(body))

def whenElse(cond: Expr)(thenBody: ScriptBuilder ?=> Unit)(elseBody: ScriptBuilder ?=> Unit)(using b: ScriptBuilder): Unit =
  b += IfElse(cond, script(thenBody), script(elseBody))


// Input
def keyDown(key: String): Expr =
  Expr.KeyDown(key)


val deltaTime: Expr = Expr.DeltaTime

// Handlers
def propagate(): Expr = Expr.Propagate

// NOTE: handleWith must be declared before perform(effect)
def handleWith(handler: Handler)(using b: ScriptBuilder): Unit =
  b += HandleWith(handler)

def handleWith(handler: Handler)(body: ScriptBuilder ?=> Unit)(using b: ScriptBuilder): Unit =
  b += HandleWith(handler, script(body))

// TODO: merge handle with handleWith dependant on where it is
def handle(handlers: Handler*)(using b: WorldBuilder): Unit =
  b.addHandler(handlers.toList)


// World building
def spawn(entity: Entity)(using b: WorldBuilder): Unit =
  b.addEntity(entity)

// TODO: Fix overloaded defaults (allowing for id and non id)
def region(id: String, x: Double, y: Double, w: Double, h: Double,
           colour: Option[(Double, Double, Double)] = None,
           condition: Option[Expr] = None,
           visual: Option[Visual] = None)
          (handlers: Handler*)
          (using b: WorldBuilder): Unit =
  b.addRegion(Region(Some(id), x, y, w, h, Behaviour.Basic(handlers.toList), condition, colour, visual))

def platform(id: String, x: Double, y: Double, w: Double, h: Double,
             colour: Option[(Double, Double, Double)] = None,
             oneWay: Boolean = false,
             condition: Option[Expr] = None,
             visual: Option[Visual] = None)
            (using b: WorldBuilder): Unit =
  b.addRegion(Region(Some(id), x, y, w, h, Behaviour.Solid(oneWay), condition, colour, visual))

// TODO: maybe remove — can be mimicked from handler values, but still needed for one-shot code
// Could use "hasBeenUnderwater" etc
def triggerable(id: String, x: Double, y: Double, w: Double, h: Double,
                colour: Option[(Double, Double, Double)] = None,
                condition: Option[Expr] = None,
                visual: Option[Visual] = None,
                onEnter: ScriptBuilder ?=> Unit = (_: ScriptBuilder) ?=> (),
                onExit:  ScriptBuilder ?=> Unit = (_: ScriptBuilder) ?=> ())
               (using b: WorldBuilder): Unit =
  b.addRegion(Region(Some(id), x, y, w, h,
    Behaviour.Trigger(script(onEnter), script(onExit)), condition, colour, visual))

// Camera
// TODO: stack camera (sometimes pressing to swap in doesnt get registered as it is dependant on which task is ran last)
// tldr: camera swapping doesnt always work
/*
  val player = entity("player")
    .onSpawn {
      setState("x", 100.0)
      setState("y", 0.0)
    }
    .onUpdate {
      when(keyDown("down"))(perform(Effect.Camera()))
      perform(Effect.Gravity())
      when(keyDown("right"))(move(5.0, 0.0))
      when(keyDown("left"))(move(-5.0, 0.0))
      when(keyDown("space"))(perform(Effect.Jump()))
    }

  val item = entity("item")
    .onSpawn {
      setState("x", 600.0)
      setState("y", 0.0)
    }
    .onUpdate {
      perform(Effect.Gravity())
      when(keyDown("up"))(perform(Effect.Camera()))
      val didCollide = collides(player)
      when(didCollide)(despawn())
    }
*/
def camera()(using b: ScriptBuilder): Unit =
  perform(Effect.Camera())


// Register an entity as a spawnable template
def template(e: Entity)(using b: WorldBuilder): Unit =
  b.addTemplate(e)

// Spawn a registered template entity at the given position
def spawnAt(templateName: String, x: Expr, y: Expr)(using b: ScriptBuilder): Unit =
  perform(Effect.SpawnAt(templateName, x, y))


// UI / Debug
// TODO: draw() planned for UI system
def draw()(using b: ScriptBuilder): Unit =
  perform(Effect.Draw())

def showState(key: String)(using b: ScriptBuilder): Unit =
  perform(Effect.ShowState(key))


// Spawn-time configuration
def setSprite(path: String)(using b: ScriptBuilder): Unit =
  b += Configure(SpawnConfig.SetSprite(path))

def setSpritesheet(path: String, frameWidth: Int, frameHeight: Int)(using b: ScriptBuilder): Unit =
  b += Configure(SpawnConfig.SetSpritesheet(path, frameWidth, frameHeight))

// Visual helpers
def stretch(path: String): Visual =
  Visual(path, VisualMode.Stretch)

def tile(path: String, size: Double = 32): Visual =
  Visual(path, VisualMode.Tile, Some((size, size)))

def tile(path: String, width: Double, height: Double): Visual =
  Visual(path, VisualMode.Tile, Some((width, height)))

def sprite(path: String): Visual =
  Visual(path, VisualMode.Sprite)

given Conversion[Visual, Option[Visual]] = Some(_)

// Observe entity state directly — only valid inside animRule conditions
def obsState(key: String): Expr = Expr.StateRead(key)

// Observe global state — valid in region conditions and animRule conditions
def obsGlobal(key: String): Expr = Expr.GlobalRead(key)


def obsStateOf(id: String, key: String): Expr = Expr.EntityRead(id, key)

def exists(id: String): Expr = Expr.EntityExists(id)

// Finds the first matching behaviour
def switchState(stateKey: String, states: (String, ScriptBuilder ?=> Unit)*)(using b: ScriptBuilder): Unit =
  if states.isEmpty then return
  val current = getState(stateKey)
  def buildChain(remaining: Seq[(String, ScriptBuilder ?=> Unit)]): Unit =
    remaining match
      case Seq((_, behaviour)) =>
        behaviour
      case (name, behaviour) +: tail =>
        whenElse(current === name) {
          behaviour
        } {
          buildChain(tail)
        }
  buildChain(states)

def max(exprs: Expr*): Expr = Expr.Max(exprs*)
def min(exprs: Expr*): Expr = Expr.Min(exprs*)

def animRule(name: String, frames: List[Int], fps: Int, flipped: Boolean)(using b: ScriptBuilder): Unit =
  b += Configure(SpawnConfig.AnimRule(name, frames, fps, condition = None, flipped))

def animRule(name: String, frames: List[Int], fps: Int, flipped: Boolean = false)(cond: Expr)(using b: ScriptBuilder): Unit =
  b += Configure(SpawnConfig.AnimRule(name, frames, fps, condition = Some(cond), flipped))