package clove.dsl.helpers

import clove.ast.*
import clove.dsl.{Entity, Behaviour, Region, ScriptBuilder, Visual, WorldBuilder, script}


// World building

def spawn(entity: Entity)(using b: WorldBuilder): Unit =
  b.addEntity(entity)

// Register an entity as a spawnable template
def template(e: Entity)(using b: WorldBuilder): Unit =
  b.addTemplate(e)

// Define a global variable in a world block
def global(key: String, value: Expr)(using b: WorldBuilder): Unit =
  b.setGlobal(key, value)

// Add a custom effect to the world
def register(effects: CustomEffect[?]*)(using b: WorldBuilder): Unit =
  b.addCustomEffects(effects.toList)

// World-level handlers (the defaults at the bottom of the resolution order)
def handle(handlers: Handler*)(using b: WorldBuilder): Unit =
  b.addHandler(handlers.toList)


// Regions

def region(id: String, x: Double, y: Double, w: Double, h: Double,
           colour: Option[(Double, Double, Double)] = None,
           condition: Option[Expr] = None,
           visual: Option[Visual] = None)
          (handlers: Handler*)
          (using b: WorldBuilder): Unit =
  b.addRegion(Region(id, x, y, w, h, Behaviour.Basic(handlers.toList), condition, colour, visual))

def platform(id: String, x: Double, y: Double, w: Double, h: Double,
             colour: Option[(Double, Double, Double)] = None,
             oneWay: Boolean = false,
             condition: Option[Expr] = None,
             visual: Option[Visual] = None)
            (using b: WorldBuilder): Unit =
  b.addRegion(Region(id, x, y, w, h, Behaviour.Solid(oneWay), condition, colour, visual))

// A region that runs scripts when an entity enters or exits it
// i believe this behaviour can be mimicked by query keys? leaving a redundancy
def triggerable(id: String, x: Double, y: Double, w: Double, h: Double,
                colour: Option[(Double, Double, Double)] = None,
                condition: Option[Expr] = None,
                visual: Option[Visual] = None,
                onEnter: ScriptBuilder ?=> Unit = (_: ScriptBuilder) ?=> (),
                onExit:  ScriptBuilder ?=> Unit = (_: ScriptBuilder) ?=> ())
               (using b: WorldBuilder): Unit =
  b.addRegion(Region(id, x, y, w, h,
    Behaviour.Trigger(script(onEnter), script(onExit)), condition, colour, visual))
