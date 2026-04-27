import clove.dsl.*
import clove.compiler.*
import clove.ast.*
import clove.dsl.given

@main def run(): Unit =


  val Drown = customEffect("Drown") { (value, dt) =>
    script {
      setState("air", getState("air") - value * dt)
    }
  }

  val Freeze = customEffect("Freeze") { (value, dt) =>
    script {
      setState("air", getState("air") - value * dt)
    }
  }

  val tset = customEffect("Drown") { (value, dt) =>
    script {
      setState("air", getState("air") - value * dt)
    }
  }

  val physics = handler("Gravity" -> 9.8, "Jump" -> 5.0, "Move" -> 1.0, 
  "Drown" -> 1.0,
  "Freeze" -> 0.0
  )

  val noGravity = handler("Gravity" -> 0.0)

  val lowGravity = handler("Gravity" -> 2.0)

  val waterDrag = handler("Move" -> 0.3 * propagate())
  val slowMotion = handler("move" -> 0.3)

  val waterRegion = handler("Gravity" -> 2.0, "Move" -> 0.3, "Drown" -> 10.0)

  val highJump = handler("Jump" -> 10.0)

  val speedBoost = handler("Move" -> 2.0)


  
  val item = entity("item")
    .onSpawn {
      setState("x", 600.0)
      setState("y", 0.0)
    }
    .onUpdate {
      perform(Effect.Gravity())
    }

  val player = entity("player")
    .onSpawn {
      setState("x", 100.0)
      setState("y", 0.0)
      setState("air", 100.0)
    }
    .onUpdate {
      perform(Effect.Camera())
      when(keyDown("up"))(handleWith(waterDrag))
      when(keyDown("lshift"))(handleWith(slowMotion))
      perform(Effect.Gravity())
      when(keyDown("right"))(move(5.0, 0.0))
      when(keyDown("left"))(move(-5.0, 0.0))
      when(keyDown("space"))(perform(Effect.Jump()))

      when(getState("x") >= 200.0 && getState("x") <= 500.0)(
        perform(Drown)
      )
      val didCollide = collides(item)
      when(didCollide)(setState("isUnderwater", false))
    }


  val setup = world {
    region(200, 0, 300, 600, (0.0, 0.5, 1.0))(waterRegion)
    // region(200, 0, 300, 600, (0.0, 0.5, 1.0))(lowGravity, waterDrag)
    spawn(player)
    spawn(item)

    handle(physics)
    register(Drown, Freeze)
  }

  val outputPath = os.pwd / "output" / "main.lua"
  LoveRuntime.writeToFile(setup, outputPath)
  println(s"Written to $outputPath")