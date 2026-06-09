import clove.dsl.*
import clove.compiler.*
import clove.compiler.runtime.*
import clove.ast.*
import clove.ast.Key.*
import clove.dsl.given

import scala.sys.process.*

def testRegions(args: String*): Unit =
  def argVal(flag: String, default: Int): Int =
    val i = args.indexOf(flag)
    if i >= 0 && i + 1 < args.length then args(i + 1).toInt else default

  val nRegions  = argVal("--regions", 100)
  val nEntities = argVal("--entities", 1)

  val tile     = 32.0
  val cols     = math.ceil(math.sqrt(nRegions.toDouble)).toInt
  val originX  = 0.0
  val originY  = 0.0

  val setup = world {
    for j <- 0 until nRegions do
      val cx = originX + (j % cols) * tile
      val cy = originY + (j / cols) * tile
      region(s"tile_$j", cx, cy, tile, tile)(handler(Gravity -> 10.0))

    for i <- 0 until nEntities do
      val e = entity(s"player_$i")
        .onSpawn {
          setState("x", originX + (i % cols) * tile + tile / 2)
          setState("y", originY + (i / cols) * tile + tile / 2)
          setSize(8.0, 8.0)
        }
        .onUpdate {
          perform(Effect.Gravity())
          move(10.0, 0.0)
        }
      spawn(e)

    handle(handler(Gravity -> 10.0, Move -> 1.0))
  }

  val outputPath = os.pwd / "output" / "main.lua"
  LoveRuntime.writeToFile(setup, outputPath, hotReload = false)

  val csvPath = (os.pwd / "results.csv").toString.replace("\\", "/")
  val wrapper =
    s"""
       |local VARIANT      = "clove_region"
       |local ENTITY_COUNT = $nEntities
       |local REGION_COUNT = $nRegions
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
       |        hf:write("variant, entities, regions, update_ms, total_frame_ms, fps,mem_kb\\n")
       |        hf:close()
       |      end
       |    else
       |      existing:close()
       |    end
       |    local f = io.open(CSV_PATH, "a")
       |    if f then
       |      f:write(string.format("%s,%d,%d,%.5f,%.5f,%.1f,%.1f\\n",
       |              VARIANT, ENTITY_COUNT, REGION_COUNT,
       |              avg_update, avg_frame, fps, mem_kb))
       |      f:close()
       |    end
       |    love.event.quit()
       |  end
       |end
       |""".stripMargin

  os.write.append(outputPath, wrapper)
  println(s"Generated region test world: $nEntities entities, $nRegions regions")

  val cmd = sys.props("os.name").toLowerCase() match
    case osName if osName.contains("windows") => Seq("cmd", "/C", "love", "output")
    case _ => Seq("love", "output")
  cmd.!