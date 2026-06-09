import clove.dsl.*
import clove.compiler.*
import clove.compiler.runtime.*
import clove.ast.*
import clove.ast.Key.*
import clove.dsl.given

import scala.sys.process.*

def testStack(args: String*): Unit =
  def argVal(flag: String, default: Int): Int =
    val i = args.indexOf(flag)
    if i >= 0 && i + 1 < args.length then args(i + 1).toInt else default
  def argStr(flag: String, default: String): String =
    val i = args.indexOf(flag)
    if i >= 0 && i + 1 < args.length then args(i + 1) else default

  val depth = argVal("--depth", 8)
  val mode  = argStr("--mode", "region_prop")

  val regionSourced = mode.startsWith("region")
  val propagating   = mode.endsWith("prop")

  def stackHandler() =
    if propagating then handler(Move -> propagate())
    else handler(Move -> 2.0)

  val player = entity("player")
    .onSpawn {
      setState("x", 100.0)
      setState("y", 100.0)
      setSize(8.0, 8.0)
    }
    .onUpdate {
      if !regionSourced then
        for _ <- 0 until depth do handleWith(stackHandler())
      move(10.0, 0.0)
    }

  val setup = world {
    if regionSourced then
      for j <- 0 until depth do
        region(s"layer_$j", 0.0, 0.0, 400.0, 400.0)(stackHandler())

    spawn(player)
    handle(handler(Move -> 1.0, Gravity -> 10.0))
  }

  val outputPath = os.pwd / "output" / "main.lua"
  LoveRuntime.writeToFile(setup, outputPath, hotReload = false)

  val csvPath = (os.pwd / "results.csv").toString.replace("\\", "/")
  val variant = s"stack_$mode"
  val wrapper =
    s"""
       |local VARIANT      = "$variant"
       |local DEPTH        = $depth
       |local CSV_PATH     = "$csvPath"
       |local WARMUP = 120
       |local SAMPLE = 600
       |local _frame = 0
       |local _total_update_ms = 0
       |local _total_frame_ms  = 0
       |local _origUpdate = love.update
       |
       |do
       |  local w, h, flags = love.window.getMode()
       |  flags.vsync = 0
       |  love.window.setMode(w, h, flags)
       |end
       |
       |function love.update(dt)
       |  local t0 = love.timer.getTime()
       |  if _origUpdate then _origUpdate(dt) end
       |  local t1 = love.timer.getTime()
       |  _frame = _frame + 1
       |  if _frame > WARMUP and _frame <= WARMUP + SAMPLE then
       |    _total_update_ms = _total_update_ms + (t1 - t0) * 1000.0
       |    _total_frame_ms  = _total_frame_ms  + dt * 1000.0
       |  elseif _frame > WARMUP + SAMPLE then
       |    local avg_update = _total_update_ms / SAMPLE
       |    local avg_frame  = _total_frame_ms  / SAMPLE
       |    local fps = (avg_frame > 0) and (1000.0 / avg_frame) or 0
       |    collectgarbage("collect")
       |    local mem_kb = collectgarbage("count")
       |    local existing = io.open(CSV_PATH, "r")
       |    if existing == nil then
       |      local hf = io.open(CSV_PATH, "w")
       |      if hf then
       |        hf:write("variant, depth, update_ms, total_frame_ms, fps, mem_kb\\n")
       |        hf:close()
       |      end
       |    else
       |      existing:close()
       |    end
       |    local f = io.open(CSV_PATH, "a")
       |    if f then
       |      f:write(string.format("%s,%d,%.5f,%.5f,%.1f,%.1f\\n",
       |              VARIANT, DEPTH, avg_update, avg_frame, fps, mem_kb))
       |      f:close()
       |    end
       |    love.event.quit()
       |  end
       |end
       |""".stripMargin

  os.write.append(outputPath, wrapper)
  println(s"Generated stack test: mode=$mode depth=$depth")

  val cmd = sys.props("os.name").toLowerCase() match
    case osName if osName.contains("windows") => Seq("cmd", "/C", "love", "output")
    case _ => Seq("love", "output")
  cmd.!