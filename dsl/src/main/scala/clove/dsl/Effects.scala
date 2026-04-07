package clove.dsl

import clove.ast.*

def keyDown(key: String): Expr = Expr.KeyDown(key)

def collides(a: Expr, b: Expr): Expr = Expr.Collides(a, b)

def move(entity: Expr, dx: Double, dy: Double)(using b: ScriptBuilder): Unit =
  b += Script.Perform(Effect.Move(entity, Expr.Num(dx), Expr.Num(dy)))

def draw(entity: Expr)(using b: ScriptBuilder): Unit =
  b += Script.Perform(Effect.Draw(entity))

def spawn(entity: Expr, withGravity: Boolean = false)(using b: ScriptBuilder): Unit =
  b += Script.Perform(Effect.Spawn(entity, withGravity))

def despawn(entity: Expr)(using b: ScriptBuilder): Unit =
  b += Script.Perform(Effect.Despawn(entity))

def setState(entity: Expr, key: String, value: Expr)(using b: ScriptBuilder): Unit =
  b += Script.Perform(Effect.SetState(entity, key, value))

def getState(entity: Expr, key: String, result: String)(using b: ScriptBuilder): Unit =
  b += Script.Perform(Effect.GetState(entity, key, result))

def when(cond: Expr)(body: ScriptBuilder ?=> Unit)(using b: ScriptBuilder): Unit =
  b += Script.When(cond, script(body))

def loop(body: ScriptBuilder ?=> Unit)(using b: ScriptBuilder): Unit =
  b += Script.Loop(script(body))

def withHandler(handler: Handler)(body: ScriptBuilder ?=> Unit)(using b: ScriptBuilder): Unit =
  b += Script.WithHandler(handler, script(body))

def region(x: Double, y: Double, w: Double, h: Double,
           color: (Double, Double, Double) = (1.0, 1.0, 1.0))
          (handlers: Handler*)
          (using b: WorldBuilder): Unit =
  b.addRegion(Region(x, y, w, h, color._1, color._2, color._3, handlers.toList))