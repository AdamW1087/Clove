package clove.dsl

import clove.ast.*

def perform(effect: Effect)(using b: ScriptBuilder): Expr =
  val varName = b.nextVar()
  b += Bind(varName, effect)
  Expr.Var(varName)

def move(dx: Double, dy: Double)(using b: ScriptBuilder): Expr =
  perform(Effect.Move(Expr.Num(dx), Expr.Num(dy)))

def jump()(using b: ScriptBuilder): Expr =
  perform(Effect.Jump())

def spawn(entity: Entity)(using b: WorldBuilder): Unit =
  b.addEntity(entity)

def despawn()(using b: ScriptBuilder): Expr =
  perform(Effect.Despawn())

// TODO
def draw()(using b: ScriptBuilder): Expr =
  perform(Effect.Draw())

def setState(key: String, value: Expr)(using b: ScriptBuilder): Expr =
  perform(Effect.SetState(key, value))

def getState(key: String)(using b: ScriptBuilder): Expr =
  perform(Effect.GetState(key))

def when(cond: Expr)(body: ScriptBuilder ?=> Unit)(using b: ScriptBuilder): Unit =
  b += If(cond, script(body))

def loop(body: ScriptBuilder ?=> Unit)(using b: ScriptBuilder): Unit =
  b += Loop(script(body))

def keyDown(key: String): Expr =
  Expr.KeyDown(key)

def collides(target: Expr)(using b: ScriptBuilder): Expr =
  perform(Effect.Collides(target))

def region(x: Double, y: Double, w: Double, h: Double,
           color: (Double, Double, Double) = (1.0, 1.0, 1.0))
          (handlers: Handler*)
          (using b: WorldBuilder): Unit =
  b.addRegion(Region(x, y, w, h, color._1, color._2, color._3, handlers.toList))


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
def camera()(using b: ScriptBuilder): Expr =
  perform(Effect.Camera())


// NOTE: handleWith must be declared before perform(effect)
def handleWith(handler: Handler)(using b: ScriptBuilder): Unit =
  b += HandleWith(handler)

def handleWith(handler: Handler)(body: ScriptBuilder ?=> Unit)(using b: ScriptBuilder): Unit =
  b += HandleWith(handler, script(body))

def propagate(): Expr = Expr.Propagate

// TODO: merge handle with handeWith dependant on where it is
def handle(handlers: Handler*)(using b: WorldBuilder): Unit =
  b.addHandler(handlers.toList)