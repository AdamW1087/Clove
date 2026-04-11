import clove.dsl.*
import clove.compiler.*
import clove.ast.*
import clove.dsl.given

@main def run(): Unit =

  val player = entity("player")
    .onSpawn {
      setState("x", 100.0)
      setState("y", 0.0)
    }
    .onUpdate {
      perform(Effect.ApplyGravity())
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
      perform(Effect.ApplyGravity())
      val didCollide = collides(player)
      when(didCollide)(despawn())
    }

  val beamGravity = gravityHandler(2.0)

  val setup = world {
    camera(follow = player, threshold = 400, axis = Axis.Horizontal)
    region(200, 0, 300, 600, (0.0, 0.5, 1.0))(beamGravity)
    spawn(player)
    spawn(item)
  }

  val outputPath = os.pwd / "output" / "main.lua"
  LoveRuntime.writeToFile(setup, outputPath)
  println(s"Written to $outputPath")