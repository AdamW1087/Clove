import clove.dsl.*
import clove.compiler.*
import clove.ast.*
import clove.dsl.given

@main def run(args: String*): Unit =
  val hotReload = args.contains("--reload")

  val Drown = customEffect("Drown") { (value, dt) =>
    script {
      setState("air", max(getState("air") - value * dt, 0.0))
    }
  }


  val physics = handler(
    "Gravity"  -> 10.0,
    "Jump"     -> 5.0,
    "Move"     -> 1.0,
    "Drown"    -> 8.0,
    "isUnderwater" -> false
  )

  val waterRegion = handler(
    "Move"         -> 0.3,
    "Drown"        -> 10.0,
    "isUnderwater" -> true,
    "Jump"         -> propagate() via { (v, dt) => script { setState("vy", -v) } }
  )

  val speedBoost = handler("Move" -> 2.0 * propagate())
  val slowMotion = handler("Move" -> 0.3 * propagate())


  val item = entity("item")
    .onSpawn {
      setState("x", 600.0)
      setState("y", 0.0)
      setSize(32.0, 32.0)
    }
    .onUpdate {
      perform(Effect.Gravity())
    }

  val player = entity("player")
    .onSpawn {
      setState("x", 100.0)
      setState("y", 0.0)
      setState("grounded", false)
      setState("facingRight", true)
      setState("air", 100.0)
      setState("health", 50.0)

      setSize(50.0, 80.0)
      setSpritesheet("assets/player_sheet.png", 64, 64)

      animRule("jumpR", frames = List(5),          fps = 1, flipped = false)(!obsState("grounded") &&  obsState("facingRight"))
      animRule("walkR", frames = List(1, 2, 3, 4), fps = 8, flipped = false)(keyDown("d"))
      animRule("idleR", frames = List(0),           fps = 1, flipped = false)(obsState("facingRight"))
      animRule("jumpL", frames = List(5),           fps = 1, flipped = true )(!obsState("grounded") && !obsState("facingRight"))
      animRule("walkL", frames = List(1, 2, 3, 4), fps = 8, flipped = true )(keyDown("a"))
      animRule("idleL", frames = List(0),           fps = 1, flipped = true )
    }
    .onUpdate {
      perform(Effect.Camera())

      when(keyDown("lshift")) { handleWith(speedBoost) }

      perform(Effect.Gravity())

      when(keyDown("d")) {
        setState("facingRight", true)
        move(500.0, 0.0)
      }
      when(keyDown("a")) {
        setState("facingRight", false)
        move(-500.0, 0.0)
      }
      when(keyDown("space")) { perform(Effect.Jump()) }

      showState("health")
      whenElse(query("isUnderwater")) {
        perform(Drown)
        showState("air")
      } {
        setState("air", 100.0)
      }
    }


  val setup = world {
    region("sky", 0, 0, 3200, 600,
      visual = Some(Visual("assets/sky.png", VisualMode.Tile, Some(32, 32))))()

    platform("ground", 0, 550, 3200, 50,
      visual = Some(Visual("assets/ground.png", VisualMode.Tile, Some(32, 32))))

    platform("bricks1", 200, 380, 160, 32,
      visual = Some(Visual("assets/brick.png", VisualMode.Tile, Some(32, 32))))

    platform("bricks2", 500, 300, 128, 32,
      visual = Some(Visual("assets/brick.png", VisualMode.Tile, Some(32, 32))))

    region("water", 600, 200, 200, 350,
      colour = Some((0.0, 0.4, 1.0)),
      visual = Some(Visual("assets/water.png", VisualMode.Tile, Some(32, 32))))(waterRegion)

    spawn(player)
    spawn(item)

    handle(physics)
    register(Drown)
  }

  val outputPath = os.pwd / "output" / "main.lua"
  LoveRuntime.writeToFile(setup, outputPath, hotReload)
  println(s"Written to $outputPath")