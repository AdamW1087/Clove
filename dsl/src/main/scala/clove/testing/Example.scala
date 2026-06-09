package clove


import clove.dsl.*
import clove.compiler.*
import clove.compiler.runtime.*
import clove.ast.*
import clove.ast.Key.*
import clove.dsl.given

@main def run(args: String*): Unit =
  val hotReload = args.contains("--reload")

  val windZone  = handler(Move -> 0.5 * propagate())
  val swampZone = handler(Move -> 0.5)
  val boots     = handler(Move -> 3.0 * propagate())
  val player = entity("player")
  .onSpawn { 
    setState("x", 100.0)
    setState("y", 100.0)
    setSize(16.0, 16.0) 
    setState("hasBoots", false)
  }
  .onUpdate {
    when(getState("hasBoots")) {handleWith(boots)}

    when(keyDown("d")){move(50.0, 0.0)}
    when(keyDown("a")){move(-50.0, 0.0)}

    when(keyDown("k")) {setState("hasBoots", !getState("hasBoots"))}
  }
  val game = world {
    region("swamp", 0,   0, 400, 400, colour = Some((0, 128, 0)))(swampZone)
    region("wind",  200, 0, 400, 400, colour = Some((0, 0, 128)))(windZone)
    spawn(player)
    handle(handler(Move -> 1.0))
  }

  val outputPath = os.pwd / "output" / "main.lua"
  LoveRuntime.writeToFile(game, outputPath, hotReload)