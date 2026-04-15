import clove.dsl.*
import clove.compiler.*
import clove.ast.*
import clove.dsl.given

@main def run(): Unit =

  val noGravity = handler("Gravity" -> 0.0)

  val lowGravity = handler("Gravity" -> 2.0)

  val waterDrag = handler("Move" -> propagate(0.3))
  val slowMotion = handler("Move" -> propagate(0.1))

  val fastRegion = handler("Gravity" -> 2.0, "Move" -> 20.0)

  val highJump = handler("Jump" -> 10.0)

  val speedBoost = handler("Move" -> 2.0)

  val player = entity("player")
    .onSpawn {
      setState("x", 100.0)
      setState("y", 0.0)
    }
    .onUpdate {
      perform(Effect.Camera())
      when(keyDown("up"))(handleWith(waterDrag))
      when(keyDown("lshift"))(handleWith(slowMotion))
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
      val didCollide = collides(player)
      when(didCollide)(despawn())
    }


  val setup = world {
    region(200, 0, 300, 600, (0.0, 0.5, 1.0))(fastRegion)
    // region(200, 0, 300, 600, (0.0, 0.5, 1.0))(lowGravity, waterDrag)
    spawn(player)
    spawn(item)
  }

  val outputPath = os.pwd / "output" / "main.lua"
  LoveRuntime.writeToFile(setup, outputPath)
  println(s"Written to $outputPath")