import clove.dsl.*
import clove.dsl.helpers.*
import clove.compiler.*
import clove.compiler.runtime.*
import clove.ast.*
import clove.ast.Key.*
import clove.dsl.given
import clove.dsl.helpers.given

@main def runDemo(args: String*): Unit =
  val hotReload = args.contains("--reload")


  val isUnderwater = QueryKey("isUnderwater")

  val Drown = customEffect("Drown") { (value, dt) =>
    script {
      setState("air", max(getState("air") - value * dt, 0.0))
    }
  }

  val water = handler(
    Jump -> 100.0 via { (resolved, dt) =>
      script { setState("vy", -resolved) }
    },
    Move -> 0.3 * propagate(),
    isUnderwater -> true,
    Drown -> 10.0
  )

  val wind = handler(
    Move -> 0.5 * propagate()
  )

  val speedBoost = handler(
    Move -> 1.5 * propagate()
  )


  val carrot = entity("carrot")
    .onSpawn{
      setState("x", 2700.0)
      setState("y", 400.0)
      setSize(40.0, 40.0)
      setSpritesheet("assets/carrot.png", 16, 16)
      setState("facingRight", true)

      animRule("walkR", frames = List(0, 1, 2, 3), fps = 4, flipped = true) (obsState("facingRight"))
      animRule("walkL", frames = List(0, 1, 2, 3), fps = 4, flipped = false)

    }
    .onUpdate{
      perform(Effect.Gravity())
      when(getState("facingRight")) {
        move(50.0, 0.0)
      } otherwise {
        move(-50.0, 0.0)
      }

      when(keyDown("f")){
        setState("x", 2700.0)
        setState("y", 400.0)
      }

      when(getState("x") - 2700.0 > 50.0 || 2700.0 - getState("x") > 50.0) {
        setState("facingRight", !getState("facingRight"))
      }
    }


  val player = entity("player")
    .onSpawn {
      setState("x", 100.0)
      setState("y", 250.0)
      setState("grounded", false)
      setState("facingRight", true)
      setState("hasBoots", false)
      setState("vy", 0.0)
      setSize(80.0, 80.0)
      setSpritesheet("assets/new.png", 32, 32)

      setState("air", 100.0)

      animRule("jumpR", frames = List(10), fps = 1, flipped = false)(!obsState("grounded") && obsState("facingRight"))

      animRule("jumpL", frames = List(10), fps = 1, flipped = true)(!obsState("grounded") && !obsState("facingRight"))


      animRule("carrotWalkR", frames = List(5, 6, 7, 8), fps = 8, flipped = false)(obsState("hasBoots") && keyDown("d"))
      animRule("carrotWalkL", frames = List(5, 6, 7, 8), fps = 8, flipped = true)(obsState("hasBoots") && keyDown("a"))

      animRule("carrotIdleR", frames = List(5), fps = 1, flipped = false)(obsState("grounded") && obsState("hasBoots") && obsState("facingRight"))
      animRule("carrotIdleL", frames = List(5), fps = 1, flipped = true)(obsState("grounded") && obsState("hasBoots") && !obsState("facingRight"))



      animRule("walkR", frames = List(1, 2, 3, 4), fps = 8, flipped = false)(keyDown("d"))
      animRule("idleR", frames = List(0), fps = 1, flipped = false)(obsState("facingRight"))
      animRule("walkL", frames = List(1, 2, 3, 4), fps = 8, flipped = true )(keyDown("a"))

      animRule("idleL", frames = List(0), fps = 1, flipped = true )
    }
    .onInit(setState("facingRight", false))
    .onUpdate {
      setCamera(zoom = 1.0, deadzoneX = 100.0, deadzoneY = 500.0)
      perform(Effect.Gravity())

      when(collides(carrot)) {
        setState("hasBoots", true)
      }
      when(keyDown("k")) { setState("hasBoots", false) }
      when(keyDown("f")){
        setState("x", 100.0)
        setState("y", 250.0)
      }

      when(getState("hasBoots")) { handleWith(speedBoost) }

      when(keyDown("d")) {
        setState("facingRight", true)
        move(500.0, 0.0)
      }
      when(keyDown("a")) {
        setState("facingRight", false)
        move(-500.0, 0.0)
      }
      when(keyDown("space")) {
        val jumped = perform(Effect.Jump())
        when(jumped) { playSound("assets/jump.mp3") }
      }

      when(isUnderwater()) {
        perform(Drown)
        uiSprites(20, 20, "assets/bubble.png",
          count = ceil(getState("air") / 20.0),
          w = 40.0, h = 40.0, spacing = 5.0)
      } otherwise {
        setState("air", 100.0)
      }

    }

  val physics = handler(
    Gravity -> 1000.0,
    Jump    -> 600.0,
    Move    -> 1.1,
    isUnderwater -> false,
    Drown -> 0.0
  )

  val setup = world {
    region("sky", -1000, -200, 10000, 1000,
      visual = stretch("assets/sky.png")) ()

    region("foliage", -1000, -50, 10000, 650,
      visual = tile("assets/back_foliage.png", 570)) ()

    region("foliage2", -1000, -50, 10000, 600,
      visual = tile("assets/front_foliage.png", 750)) ()

    platform("barrierL", 0, 0, 1, 600)
    platform("barrierR", 8000, 0, 1, 600)


    region("water", 600, 300, 330, 250,
      colour = Some((0.0, 0.4, 1.0)),
      visual = tile("assets/water.png", 32)) (water)

    platform("waterwallL", 580, 300, 32, 250,
      visual = tile("assets/block.png", 32))
    platform("waterwallR", 920, 300, 32, 250,
      visual = tile("assets/block.png", 32))


    region("wind", 2100, 155, 320, 400,
      visual = tile("assets/wind.png", 64)) (wind)


    region("water2", 3600, 400, 330, 250,
      colour = Some((0.0, 0.4, 1.0)),
      visual = tile("assets/water.png", 32)) (water)

    platform("waterwallL2", 3580, 400, 32, 250,
      visual = tile("assets/block.png", 32))
    platform("waterwallR2", 3920, 400, 32, 250,
      visual = tile("assets/block.png", 32))

    region("wind2", 3600, 400, 330, 250,
      visual = tile("assets/wind.png", 64)) (wind)


    region("honey", 5100, 400, 330, 250,
      visual = tile("assets/honey.png", 32)) (water, wind)

    platform("honeywallL", 5080, 400, 32, 250,
      visual = tile("assets/block.png", 32))
    platform("honeyWallR", 5420, 400, 32, 250,
      visual = tile("assets/block.png", 32))

    platform("ground", -1000, 545, 10000, 50,
      visual = tile("assets/ground.png", 32))

    region("dirt", -1000, 575, 10000, 600,
      visual = tile("assets/dirt.png", 32)) ()


    platform("p1", 400, 425, 96, 10,
      visual = tile("assets/platform.png", 32))


    spawn(player)
    spawn(carrot)
    handle(physics)

    register(Drown)
  }

  val outputPath = os.pwd / "output" / "main.lua"
  LoveRuntime.writeToFile(setup, outputPath, hotReload)
  println(s"Written to $outputPath")


// https://shouhan.itch.io/simple-assetpack
// https://www.deviantart.com/trarian/art/Water-Texture-8-bit-905860542
// https://www.vecteezy.com/vector-art/49593703-chill-and-cool-wind-pixel-art