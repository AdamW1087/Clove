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

// WithHandler TODO

def camera(follow: Entity, threshold: Double, axis: Axis = Axis.Horizontal)
          (using b: WorldBuilder): Unit =
  b.setCamera(CameraConfig(follow, threshold, axis))