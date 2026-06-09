local ENTITY_COUNT = tonumber(os.getenv("ENTITY_COUNT")) or 500
local CSV_PATH     = os.getenv("CSV") or "results.csv"

local GRAVITY    = 10.0
local MOVE_SPEED = 50.0
local GROUND_Y   = 450.0

local entities = {}

local WARMUP = 120
local SAMPLE = 600
local _frame = 0
local _total_update_ms = 0
local _total_frame_ms  = 0

function love.load()
  for i = 0, ENTITY_COUNT - 1 do
    entities[#entities + 1] = {
      x = 50.0 + (i % 40) * 10.0,
      y = 10.0 + math.floor(i / 40) * 10.0,
      w = 8.0, h = 8.0,
      vy = 0.0,
      goRight = true,
    }
  end
  local w, h, flags = love.window.getMode()
  flags.vsync = 0
  love.window.setMode(w, h, flags)
end

local function updateEntities(dt)
  for _, e in ipairs(entities) do
    e.vy = e.vy + GRAVITY
    e.y = e.y + e.vy * dt
    if e.y + e.h >= GROUND_Y then
      e.y = GROUND_Y - e.h
      e.vy = 0.0
    end
    if e.x <= 50.0  then e.goRight = true  end
    if e.x >= 150.0 then e.goRight = false end
    if e.goRight then
      e.x = e.x + MOVE_SPEED * dt
    else
      e.x = e.x - MOVE_SPEED * dt
    end
  end
end

function love.update(dt)
  local t0 = love.timer.getTime()
  updateEntities(dt)
  local t1 = love.timer.getTime()

  _frame = _frame + 1
  if _frame > WARMUP and _frame <= WARMUP + SAMPLE then
    _total_update_ms = _total_update_ms + (t1 - t0) * 1000.0
    _total_frame_ms  = _total_frame_ms  + dt * 1000.0
  elseif _frame > WARMUP + SAMPLE then
    local avg_update = _total_update_ms / SAMPLE
    local avg_frame  = _total_frame_ms  / SAMPLE
    local fps = (avg_frame > 0) and (1000.0 / avg_frame) or 0
    collectgarbage("collect")
    local mem_kb = collectgarbage("count")
    local existing = io.open(CSV_PATH, "r")
    if existing == nil then
      local hf = io.open(CSV_PATH, "w")
      if hf then
        hf:write("variant, entities, regions, update_ms, total_frame_ms, fps,mem_kb\n")
        hf:close()
      end
    else
      existing:close()
    end
    local f = io.open(CSV_PATH, "a")
    if f then
      f:write(string.format("%s,%d,%d,%.5f,%.5f,%.1f,%.1f\n",
              "entity", ENTITY_COUNT, 0, avg_update, avg_frame, fps, mem_kb))
      f:close()
    end
    love.event.quit()
  end
end

function love.draw()
  for _, e in ipairs(entities) do
    love.graphics.rectangle("fill", e.x, e.y, e.w, e.h)
  end
end