import clove.dsl.*
import clove.compiler.*
import clove.ast.*
import clove.dsl.given

@main def run(args: String*): Unit =
  val hotReload = args.contains("--reload")

  def patrolEnemy(name: String, spawnX: Double, spawnY: Double,
                  speed: Double = 150.0,
                  turnRange: Double = 200.0): Entity =
    entity(name)
      .onSpawn {
        setState("x", spawnX)
        setState("y", spawnY)
        setState("direction", 1.0)
        setState("startX", spawnX)
        setSize(32.0, 32.0)
      }
      .onUpdate {
        perform(Effect.Gravity())

        val dir    = getState("direction")
        val x      = getState("x")
        val startX = getState("startX")

        when(turnRange > 0.0) {
          when((x - startX) > turnRange) { setState("direction", -1.0) }
          when((startX - x) > turnRange) { setState("direction",  1.0) }
        }

        whenElse(dir === 1.0) {
          move(speed, 0.0)
        } {
          move(-speed, 0.0)
        }

        when(collides(Expr.Var("player"))) {
          despawn()
        }
      }


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


  val goomba  = patrolEnemy("goomba",  spawnX = 400.0, spawnY = 0.0, speed = 120.0, turnRange = 150.0)
  val goomba2 = patrolEnemy("goomba2", spawnX = 700.0, spawnY = 0.0, speed = 100.0, turnRange = 100.0)


  def patrol(speed: Double = 150.0, range: Double = 150.0): ScriptBuilder ?=> Unit = {
    val x      = getState("x")
    val startX = getState("startX")
    when((x - startX) > range) { setState("direction", -1.0) }
    when((startX - x) > range) { setState("direction",  1.0) }
    whenElse(getState("direction") === 1.0) {
      move(speed, 0.0)
    } {
      move(-speed, 0.0)
    }
  }

  def chasePlayer(speed: Double = 250.0, chaseRange: Double = 200.0, leashRange: Double = 300.0): ScriptBuilder ?=> Unit = {
    when(exists("player")) {
      whenElse(getStateOf("player", "x") > getState("x")) {
        move(speed, 0.0)
      } {
        move(-speed, 0.0)
      }
      when(collides(Expr.Var("player"))) {
        setStateOf("player", "health", getStateOf("player", "health") - 10.0)
      }
      val dx = getStateOf("player", "x") - getState("x")
      when((dx * dx) > (leashRange * leashRange)) {
        setState("mode", "patrol")
      }
    }
    when(!exists("player")) { setState("mode", "patrol") }
  }

  def switchToChaseWhenClose(range: Double = 200.0): ScriptBuilder ?=> Unit = {
    when(exists("player")) {
      val dx = getStateOf("player", "x") - getState("x")
      when((dx * dx) < (range * range)) {
        setState("mode", "chase")
      }
    }
  }


  val item = entity("item")
    .onSpawn {
      setState("x", 300.0)
      setState("y", 0.0)
      setState("direction", 1.0)
      setState("startX", 300.0)
      setState("mode", "patrol")
      setSize(32.0, 32.0)
    }
    .onUpdate {
      perform(Effect.Gravity())

      switchState("mode",
        "patrol" -> { patrol(); switchToChaseWhenClose() },
        "chase"  -> chasePlayer()
      )
    }

  val player = entity("player")
    .onSpawn {
      setState("x", 100.0)
      setState("y", 0.0)
      setState("grounded", false)
      setState("facingRight", true)
      setState("air", 100.0)
      setState("health", 50.0)
      setState("shootCooldown", 0.0)

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

      when(getState("health") <= 0.0) {
        despawn()
      }

      when(getState("shootCooldown") > 0.0) {
        setState("shootCooldown",  max(getState("shootCooldown") - 1.0 * Expr.DeltaTime, 0.0))
      }

      when(keyDown("k") && getState("shootCooldown") === 0.0) {
        spawnAt("bullet", getState("x"), getState("y"))
        setState("shootCooldown", 3.0)
      }
    }


  val bullet = entity("bullet")
    .onSpawn {
      setSize(12.0, 6.0)
    }
    .onUpdate {
      move(500.0, 0.0)
    }


  val setup = world {
    region("sky", 0, 0, 3200, 600,
      visual = tile("assets/sky.png", 32)) ()

    platform("ground", 0, 550, 3200, 50,
      visual = tile("assets/ground.png", 32))

    platform("bricks1", 200, 380, 160, 32,
      visual = tile("assets/brick.png", 32))

    platform("bricks2", 500, 300, 128, 32,
      visual = tile("assets/brick.png", 32))

    region("water", 600, 200, 200, 350,
      colour = Some((0.0, 0.4, 1.0)),
      visual = tile("assets/water.png", 32)) (waterRegion)

    spawn(player)
    // spawn(item)
    spawn(goomba)
    spawn(goomba2)

    template(bullet)

    handle(physics)
    register(Drown)
  }

  val outputPath = os.pwd / "output" / "main.lua"
  LoveRuntime.writeToFile(setup, outputPath, hotReload)
  println(s"Written to $outputPath")