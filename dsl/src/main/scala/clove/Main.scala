import clove.dsl.{entity, setState, script, spawn, despawn, move, draw, when, keyDown, collides}
import clove.dsl.given
import clove.compiler.*
import clove.ast.*
import os.*

@main def run(): Unit =
  val player = entity("player")

  val item = entity("item")
  
  val setup = script {
    spawn(player)
    spawn(item)
    setState(item, "x", Expr.Num(300.0))
    setState(item, "y", Expr.Num(300.0))
  }
  
  val update = script {
    when(keyDown("right"))(move(player, 5.0, 0.0))
    when(keyDown("left"))(move(player, -5.0, 0.0))
    when(keyDown("up"))(move(player, 0.0, -5.0))
    when(keyDown("down"))(move(player, 0.0, 5.0))
    
    when(collides(player, item))(despawn(item))
  }

  val outputPath = os.pwd / "output" / "main.lua"
  LoveRuntime.writeToFile(setup, update, outputPath)
  println(s"Written to $outputPath")