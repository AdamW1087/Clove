import clove.dsl.*
import clove.compiler.*
import clove.ast.*
import clove.dsl.given

@main def run(): Unit =


  val Drown = customEffect("Drown") { (value, dt) =>
    script {
      setState("air", max(getState("air") - value * dt, 0.0))
    }
  }

  val Freeze = customEffect("Freeze") { (value, dt) =>
    script {
      setState("air", getState("air") - value * dt)
    }
  }

  val physics = handler("Gravity" -> 9.8, "Jump" -> 5.0, "Move" -> 1.0, 
  "Drown" -> 0.0,
  "Freeze" -> 0.0,
  "isUnderwater" -> false
  )


  val lowGravity = handler("Gravity" -> 2.0)

  val waterDrag = handler("Move" -> 0.3 * propagate())
  val slowMotion = handler("move" -> 0.3)

  val waterRegion = handler("Move" -> 0.3, 
                            "Drown" -> 50.0, 
                            "isUnderwater" -> true,
                            "Jump" -> 5.0 via 
                            { (v, dt) => script { setState("vy", -v) } }
                            )

  val highJump = handler("Jump" -> 10.0)

  val speedBoost = handler("Move" -> 2.0)


  
  val item = entity("item")
    .onSpawn {
      setState("x", 600.0)
      setState("y", 0.0)
      setSize(50.0, 50.0)
      setState("temp", 50.0)
    }
    .onUpdate {
      perform(Effect.Gravity())
    }

  val player = entity("player")
    .onSpawn {
      setState("x", 100.0)
      setState("y", 0.0)
      setState("grounded", false)
      setSize(50.0, 80.0)
      setState("facingRight", true)
      setSpritesheet("assets/player_sheet.png", 64, 64)
      setState("air", 100.0)

      setState("temp", 50.0)
      setState("health", 50.0)

      animRule("jumpR", frames = List(5), fps = 1)(!obsState("grounded") && obsState("facingRight"))
      animRule("walkR", frames = List(1, 2, 3, 4), fps = 8)(keyDown("d"))
      animRule("idleR", frames = List(0), fps = 1)(obsState("facingRight"))
      animRule("jumpL", frames = List(11), fps = 1)(!obsState("grounded") && !obsState("facingRight"))
      animRule("walkL", frames = List(7, 8, 9, 10), fps = 8)(keyDown("a"))
      animRule("idleL", frames = List(6), fps = 1)(!obsState("facingRight"))
    }
    .onUpdate {

      perform(Effect.Camera())

      when(keyDown("w")) {
        handleWith(waterDrag)
      }


      when(keyDown("lshift")) {handleWith(slowMotion)}
      perform(Effect.Gravity())
      when(keyDown("d")){
        setState("facingRight", true)
        move(500.0, 0.0)
        }
      when(keyDown("a")){
        setState("facingRight", false)
        move(-500.0, 0.0)
        }
      when(keyDown("space")) {
        // setSize(100.0, 100.0)
        perform(Effect.Jump())
      }

      when(keyDown("k")) {
        setSize(50.0, 80.0)
      }

      showState("health")
      whenElse(query("isUnderwater")) {
        perform(Drown)
        showState("air")
      } {
        setState("air", 100.0)
      }
    }


  val setup = world {
    region("test23", 200, 0, 300, 600, Some(0.0, 0.5, 1.0))(waterRegion)
    
    platform("test", 200, 400, 150, 100
    , Some(0.0, 0.0, 0.0)
    )
    spawn(player)
    spawn(item)

    global("testValue", 50.0)
    handle(physics)
    register(Drown, Freeze)
  }

  val outputPath = os.pwd / "output" / "main.lua"
  LoveRuntime.writeToFile(setup, outputPath)
  println(s"Written to $outputPath")