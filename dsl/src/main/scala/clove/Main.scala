import clove.dsl.{entity, setState, script, spawn, despawn, move, draw, when, keyDown, collides, region}
import clove.dsl.given
import clove.compiler.*
import clove.ast.*
import os.*
import clove.dsl.gravityHandler
import clove.dsl.{world}

@main def run(): Unit =
  val player = entity("player")

  val item = entity("item")
  
  val beamGravity = gravityHandler(2.0)

  val setup = world {
    spawn(player, withGravity = true)
    spawn(item, withGravity = true)
    setState(item, "x", Expr.Num(300.0))
    setState(item, "y", Expr.Num(0.0))
    region(200, 0, 300, 600, (0.0, 0.5, 1.0))(beamGravity)
  }

  val update = script {
    when(keyDown("right"))(move(player, 5.0, 0.0))
    when(keyDown("left"))(move(player, -5.0, 0.0))
    when(collides(player, item))(despawn(item))
  }

  val outputPath = os.pwd / "output" / "main.lua"
  LoveRuntime.writeToFile(setup, update, outputPath)
  println(s"Written to $outputPath")