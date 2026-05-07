package clove.dsl
import clove.dsl.worldbuilder.WorldBuilder

import clove.ast.*

// Performs an effect, discarding the return value
def perform(effect: Effect)(using b: ScriptBuilder): Unit =
  b += Perform(effect)

// Performs an effect and binds the return value to a variable
def bind(effect: Effect)(using b: ScriptBuilder): Expr =
  val varName = b.nextVar()
  b += Bind(varName, effect)
  Expr.Var(varName)


// Physics effects
def move(dx: Double, dy: Double)(using b: ScriptBuilder): Unit =
  perform(Effect.Move(Expr.Num(dx), Expr.Num(dy)))

def jump()(using b: ScriptBuilder): Unit =
  perform(Effect.Jump())

def despawn()(using b: ScriptBuilder): Unit =
  perform(Effect.Despawn())

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

// Define a global variable in world block
def global(key: String, value: Expr)(using b: WorldBuilder): Unit =
  b.setGlobal(key, value)


// Queries and custom effects
def query(name: String)(using b: ScriptBuilder): Expr =
  bind(Effect.Query(name))

def collides(target: Expr)(using b: ScriptBuilder): Expr =
  bind(Effect.Collides(target))

def customEffect(name: String)(impl: (Expr, Expr) => Script): Effect.Custom =
  Effect.Custom(name, impl)

def register(effects: Effect.Custom*)(using b: WorldBuilder): Unit =
  b.addCustomEffects(effects.toList)


// Control flow
def when(cond: Expr)(body: ScriptBuilder ?=> Unit)(using b: ScriptBuilder): Unit =
  b += If(cond, script(body))

def whenElse(cond: Expr)(thenBody: ScriptBuilder ?=> Unit)(elseBody: ScriptBuilder ?=> Unit)(using b: ScriptBuilder): Unit =
  b += IfElse(cond, script(thenBody), script(elseBody))

def loop(body: ScriptBuilder ?=> Unit)(using b: ScriptBuilder): Unit =
  b += Loop(script(body))


// Input
def keyDown(key: String): Expr =
  Expr.KeyDown(key)


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
           condition: Option[Expr] = None)
          (handlers: Handler*)
          (using b: WorldBuilder): Unit =
  b.addRegion(Region(Some(id), x, y, w, h, Behaviour.Basic(handlers.toList), condition, colour))

def platform(id: String, x: Double, y: Double, w: Double, h: Double,
             colour: Option[(Double, Double, Double)] = None,
             oneWay: Boolean = false,
             condition: Option[Expr] = None)
            (using b: WorldBuilder): Unit =
  b.addRegion(Region(Some(id), x, y, w, h, Behaviour.Solid(oneWay), condition, colour))

// TODO: maybe remove — can be mimicked from handler values, but still needed for one-shot code
// Could use "hasBeenUnderwater" etc
def triggerable(id: String, x: Double, y: Double, w: Double, h: Double,
                colour: Option[(Double, Double, Double)] = None,
                condition: Option[Expr] = None,
                onEnter: ScriptBuilder ?=> Unit = (_: ScriptBuilder) ?=> (),
                onExit:  ScriptBuilder ?=> Unit = (_: ScriptBuilder) ?=> ())
               (using b: WorldBuilder): Unit =
  b.addRegion(Region(Some(id), x, y, w, h,
    Behaviour.Trigger(script(onEnter), script(onExit)), condition, colour))

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

// Observe entity state directly — only valid inside animRule conditions
def obsState(key: String): Expr = Expr.StateRead(key)

// Observe global state — valid in region conditions and animRule conditions
def obsGlobal(key: String): Expr = Expr.GlobalRead(key)

def max(exprs: Expr*): Expr = Expr.Max(exprs*)
def min(exprs: Expr*): Expr = Expr.Min(exprs*)

// TODO: add flip (e.g. animRule(..., flipped = true)) for horizontal flipping
def animRule(name: String, frames: List[Int], fps: Int)(using b: ScriptBuilder): Unit =
  b += Configure(SpawnConfig.AnimRule(name, frames, fps, condition = None))

def animRule(name: String, frames: List[Int], fps: Int)(cond: Expr)(using b: ScriptBuilder): Unit =
  b += Configure(SpawnConfig.AnimRule(name, frames, fps, condition = Some(cond)))