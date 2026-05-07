package clove.compiler

import clove.ast.*
import clove.dsl.{World, Region, Behaviour}

object LoveRuntime:

  val groundLevel = 600

  def wrap(world: World): String =

    // Region table
    val regionTable = world.regions.zipWithIndex.map { (r, idx) =>
      val idStr = r.id.map(id => s"\"$id\"").getOrElse(s"\"region_$idx\"")
      val condStr = r.condition match
        case None       => "nil"
        case Some(expr) => s"function(globals) return ${LuaEmitter.emitCondExpr(expr)} end"

      r.behaviour match
        case Behaviour.Basic(handlers) =>
          val overrides = handlers.flatMap(_.handles).map { (k, v) =>
            s"${k.toLowerCase} = ${LuaEmitter.emitExpr(v)}"
          }.mkString(", ")
          val implOverrides = handlers.flatMap(_.impls).map { (k, f) =>
            s"${k.toLowerCase}_impl = ${LuaEmitter.emitHandlerImpl(f)}"
          }.mkString(", ")
          val allFields = List(overrides, implOverrides).filter(_.nonEmpty).mkString(", ")
          val hasHandlers = handlers.exists(h => h.handles.nonEmpty || h.impls.nonEmpty)
          val (cr, cg, cb) = r.colour.getOrElse((1.0, 1.0, 1.0))
          s"""  {id = $idStr, x = ${r.x}, y = ${r.y}, w = ${r.w}, h = ${r.h},
             |   type = "basic", hasColor = ${r.colour.isDefined.toString}, r = $cr, g = $cg, b = $cb,
             |   hasHandlers = ${hasHandlers.toString}, $allFields, condition = $condStr}""".stripMargin

        case Behaviour.Solid(oneWay) =>
          val (cr, cg, cb) = r.colour.getOrElse((1.0, 1.0, 1.0))
          s"""  {id = $idStr, x = ${r.x}, y = ${r.y}, w = ${r.w}, h = ${r.h},
             |   type = "solid", oneWay = ${oneWay.toString},
             |   hasColor = ${r.colour.isDefined.toString}, r = $cr, g = $cg, b = $cb,
             |   condition = $condStr}""".stripMargin

        case Behaviour.Trigger(onEnter, onExit) =>
          val enterLua = LuaEmitter.emitTriggerScript(onEnter)
          val exitLua  = LuaEmitter.emitTriggerScript(onExit)
          val (cr, cg, cb) = r.colour.getOrElse((1.0, 1.0, 1.0))

          s"""  {id = $idStr, x = ${r.x}, y = ${r.y}, w = ${r.w}, h = ${r.h},
             |   type = "trigger",
             |   hasColor = ${r.colour.isDefined.toString}, r = $cr, g = $cg, b = $cb,
             |   onEnter = function(task_id, globals) $enterLua end,
             |   onExit  = function(task_id, globals) $exitLua end,
             |   condition = $condStr}""".stripMargin

    }.mkString(",\n")

    // Globals table
    val globalsTable = world.initialGlobals.map { (k, v) =>
      s"  [\"$k\"] = ${LuaEmitter.emitExpr(v)}"
    }.mkString(",\n")

    val coroutines = world.entities
      .filter(_.updateScript.statements.nonEmpty)
      .map { e =>
        s"local ${e.name}Script = ${LuaEmitter.emitCoroutine(e.name, e.updateScript)}"
      }.mkString("\n")

    val taskSetup = world.entities
      .filter(_.updateScript.statements.nonEmpty)
      .map { e =>
        s"""  table.insert(tasks, {id = "${e.name}", co = ${e.name}Script, handlerStack = {}})"""
      }.mkString("\n")

    val spawnSetup = world.entities.map { e =>
      s"""  entities["${e.name}"] = {vy = 0}
         |${LuaEmitter.emitSpawnScript(e.name, e.spawnScript)}""".stripMargin
    }.mkString("\n")

    val defaultHandlerTable = world.defaultHandlers.map { h =>
      val values = h.handles.map { (k, v) =>
        s"  ${k.toLowerCase} = ${LuaEmitter.emitExpr(v)}"
      }
      val impls = h.impls.map { (k, f) =>
        s"  ${k.toLowerCase}_impl = ${LuaEmitter.emitHandlerImpl(f)}"
      }
      (values ++ impls).mkString(",\n")
    }.mkString(",\n")

    val helperFunctions =
      s"""local function clove_getState(entityId, key)
        |  return entities[entityId] and entities[entityId][key]
        |end
        |
        |local function clove_setState(entityId, key, value)
        |  if entities[entityId] then
        |    entities[entityId][key] = value
        |  end
        |end
        |
        |local function clove_findAnim(anims, name)
        |  for _, a in ipairs(anims) do
        |    if a.name == name then return a end
        |  end
        |  return anims[#anims]
        |end
        |
        |local function regionActive(region)
        |  if region.condition == nil then return true end
        |  return region.condition(globals)
        |end""".stripMargin

    val effectImpls = world.customEffects.map { e =>
      s"""  ${e.name.toLowerCase} = ${LuaEmitter.emitEffectImpl(e)}"""
    }.mkString(",\n")

    val resolveFunction =
      s"""-- Returns resolved value and first impl found during the walk (if any)
        |local function resolve(task, entityRegions, key, i, foundImpl)
        |  local taskRegions = entityRegions[task.id] or {}
        |  local stackSize = #task.handlerStack
        |  local regionSize = #taskRegions
        |  i = i or (stackSize + regionSize)
        |  foundImpl = foundImpl or nil
        |
        |  while i >= 1 do
        |    local entry, entryImpl
        |    if i > regionSize then
        |      entry     = task.handlerStack[i - regionSize][key]
        |      entryImpl = task.handlerStack[i - regionSize][key .. "_impl"]
        |    else
        |      entry     = taskRegions[i] and taskRegions[i][key]
        |      entryImpl = taskRegions[i] and taskRegions[i][key .. "_impl"]
        |    end
        |
        |    if foundImpl == nil and entryImpl ~= nil then
        |      foundImpl = entryImpl
        |    end
        |
        |    if entry ~= nil then
        |      if type(entry) == "table" and entry.propagate then
        |        local rest, restImpl = resolve(task, entityRegions, key, i - 1, foundImpl)
        |        if foundImpl == nil then foundImpl = restImpl end
        |
        |        local neutral = (entry.op == "+" or entry.op == "-") and 0.0 or 1.0
        |        local a = entry.leftVal and entry.value or (rest or neutral)
        |        local b = entry.leftVal and (rest or neutral) or entry.value
        |
        |        if entry.op == "*" then return a * b, foundImpl
        |        elseif entry.op == "+" then return a + b, foundImpl
        |        elseif entry.op == "-" then return a - b, foundImpl
        |        elseif entry.op == "/" then return a / b, foundImpl
        |        end
        |      else
        |        return entry, foundImpl
        |      end
        |    end
        |    i = i - 1
        |  end
        |
        |  local defaultImpl = defaultHandlers[key .. "_impl"]
        |  if foundImpl == nil then foundImpl = defaultImpl end
        |  if defaultHandlers[key] ~= nil then return defaultHandlers[key], foundImpl end
        |  return nil, foundImpl
        |end""".stripMargin

    val animUpdateLoop =
      s"""-- Animated sprite update
        |  for id, e in pairs(entities) do
        |    if e.anims and #e.anims > 0 then
        |      -- Find current animation
        |      local activeName = nil
        |      for _, rule in ipairs(e.anims) do
        |        if rule.condition(e) then
        |          activeName = rule.name
        |          break
        |        end
        |      end
        |
        |      -- Reset timer and frame if changing animation
        |      if activeName and activeName ~= e.currentAnim then
        |        e.currentAnim = activeName
        |        e.animFrame = 1
        |        e.animTimer = 0
        |      end
        |
        |      -- Advance frame
        |      if e.currentAnim then
        |        local anim = clove_findAnim(e.anims, e.currentAnim)
        |        e.animTimer = e.animTimer + dt
        |        if e.animTimer >= (1.0 / anim.fps) then
        |          e.animFrame = (e.animFrame % #anim.frames) + 1
        |          e.animTimer = 0
        |        end
        |      end
        |    end
        |  end""".stripMargin

    s"""-- Generated by Clove

local entities = {}
local tasks = {}
local camera = {x = 0, y = 0, follow = nil, threshold = 400}

local globals = {
$globalsTable
}

$helperFunctions

local defaultHandlers = {
$defaultHandlerTable
}

local regions = {
$regionTable
}


local uiDrawList = {}
-- overlap[entityId][regionId] = bool, tracks previous frame overlap for triggers
local prevOverlap = {}

local function checkCollision(a, b)
  if not a or not b then return false end
  return a.x < b.x + b.width and
         a.x + a.width > b.x and
         a.y < b.y + b.height and
         a.y + a.height > b.y
end

local function insideRegion(e, r)
  if not e then return false end
  return e.x < r.x + r.w and
         e.x + e.width > r.x and
         e.y < r.y + r.h and
         e.y + e.height > r.y
end

local function getEffectScopeRegions(e)
  local result = {}
  for _, region in ipairs(regions) do
    if region.type == "basic" and region.hasHandlers and regionActive(region) and insideRegion(e, region) then
      table.insert(result, region)
    end
  end
  return result
end

$resolveFunction

local function handleGravity(task, entityRegions, dt)
  local g = resolve(task, entityRegions, "gravity")
  local e = entities[task.id]
  if not e or not g then return end

  e.vy = (e.vy or 0) + g * dt
  e.y  = e.y + e.vy

  -- Solid region collision (vertical)
  for _, region in ipairs(regions) do
    if region.type == "solid" and regionActive(region) then
      if insideRegion(e, region) then
        -- Falling onto solid from above
        if e.vy >= 0 and not region.oneWay then
          e.y  = region.y - e.height
          e.vy = 0
          e.grounded = true

        -- One way (only collide if from above)
        elseif e.vy >= 0 and region.oneWay then
          local prevBottom = (e.y - e.vy * dt) + e.height
          if prevBottom <= region.y + 2 then
            e.y  = region.y - e.height
            e.vy = 0
            e.grounded = true
          end

        -- Hitting ceiling
        elseif e.vy < 0 and not region.oneWay then
          e.y  = region.y + region.h
          e.vy = 0
        end
      end
    end
  end

  -- Ground floor
  if e.y + e.height >= $groundLevel then
    e.y  = $groundLevel - e.height
    e.vy = 0
    e.grounded = true
  elseif e.y <= 0 then
    e.y  = 0
    e.vy = 0
  
  -- Not grounded if not on a solid
  else
    local onSolid = false
    for _, region in ipairs(regions) do
      if region.type == "solid" and regionActive(region) then
        if math.abs((e.y + e.height) - region.y) < 2 and
           e.x + e.width > region.x and e.x < region.x + region.w then
          onSolid = true; break
        end
      end
    end
    if not onSolid then e.grounded = false end
  end
end

local function handleMove(task, entityRegions, a, b, dt)
  local e = entities[task.id]
  if not e then return end
  local dx = a * dt
  local dy = b * dt
  local mult = resolve(task, entityRegions, "move")
  if mult then dx = dx * mult; dy = dy * mult end

  e.x = e.x + dx
  e.y = e.y + dy

  -- Solid region collision (horizontal)
  for _, region in ipairs(regions) do
    if region.type == "solid" and not region.oneWay and regionActive(region) then
      if insideRegion(e, region) then
        if dx > 0 then
          e.x = region.x - e.width
        elseif dx < 0 then
          e.x = region.x + region.w
        end

        if dy > 0 then
          e.y = region.y - e.height
        elseif dy < 0 then
          e.y = region.y + region.h
        end

      end
    end
  end
end

local function handleSetSize(task, a, b)
  local e = entities[task.id]
  if e then 
    e.width = a
    e.height = b 
  end
end

local function handleCollides(task, targetID)
  local e = entities[task.id]
  local target = entities[targetID]
  if e and target then return checkCollision(e, target) end
  return false
end

local function handleTriggers(dt)
  for id, e in pairs(entities) do
    if not prevOverlap[id] then prevOverlap[id] = {} end
    for _, region in ipairs(regions) do
      if region.type == "trigger" and regionActive(region) then
        local rid = region.id
        local isInside = insideRegion(e, region)
        local wasInside = prevOverlap[id][rid] or false
        if isInside and not wasInside then
          region.onEnter(id, globals)
        elseif not isInside and wasInside then
          region.onExit(id, globals)
        end
        prevOverlap[id][rid] = isInside
      end
    end
  end
end

local effect_impls = {
$effectImpls
}

$coroutines

function love.load()
$spawnSetup
$taskSetup

  for name, e in pairs(entities) do
    -- Static sprite
    if e.spritePath then
      e.sprite = love.graphics.newImage(e.spritePath)
    end

    -- Spritesheet: load image and build quad table
    if e.sheetPath then
      e.sheet = love.graphics.newImage(e.sheetPath)
      local sheetW = e.sheet:getWidth()
      local sheetH = e.sheet:getHeight()
      local cols = math.floor(sheetW / e.frameWidth)
      local rows = math.floor(sheetH / e.frameHeight)
      e.quads = {}
      for row = 0, rows - 1 do
        for col = 0, cols - 1 do
          -- quads are 1-indexed matching +1 offset in AnimRule emission
          local idx = row * cols + col + 1
          e.quads[idx] = love.graphics.newQuad(
            col * e.frameWidth, row * e.frameHeight,
            e.frameWidth, e.frameHeight,
            sheetW, sheetH)
        end
      end

      -- Initialise animation state if not set by spawn script
      if not e.currentAnim and e.anims and #e.anims > 0 then
        e.currentAnim = e.anims[#e.anims].name -- fallback to last rule
        e.animFrame   = 1
        e.animTimer   = 0
      end
    end
  end
end

function love.update(dt)
  local entityRegions = {}
  uiDrawList = {}
  for id, e in pairs(entities) do
    entityRegions[id] = getEffectScopeRegions(e)
  end

  for _, task in ipairs(tasks) do
    task.handlerStack = {}
    local ok, effect, a, b = coroutine.resume(task.co)

    while effect ~= nil do
      local response = nil

      if effect == "PushHandler" then
        table.insert(task.handlerStack, a)

      elseif effect == "Gravity" then
        local resolved, impl = resolve(task, entityRegions, "gravity")
        if impl then
          impl(task.id, resolved, dt)
        else
          handleGravity(task, entityRegions, dt)
        end

      elseif effect == "Move" then
        local resolved, impl = resolve(task, entityRegions, "move")
        if impl then
          impl(task.id, resolved, dt)
        else
          handleMove(task, entityRegions, a, b, dt)
        end

      elseif effect == "Jump" then
        local resolved, impl = resolve(task, entityRegions, "jump")
        if impl then
          impl(task.id, resolved, dt)
        else
          local e = entities[task.id]
          if e and e.grounded then
            e.vy = -resolved
            e.grounded = false
          end
        end

      elseif effect == "Despawn" then
        entities[task.id] = nil
        task.dead = true
        break

      elseif effect == "Collides" then
        response = handleCollides(task, a)

      elseif effect == "SetSize" then
        handleSetSize(task, a, b)

      elseif effect == "SetState" then
        if entities[task.id] then entities[task.id][a] = b end

      elseif effect == "GetState" then
        if entities[task.id] then response = entities[task.id][a] end

      elseif effect == "SetGlobal" then
        globals[a] = b

      elseif effect == "GetGlobal" then
        response = globals[a]

      elseif effect == "Camera" then
        camera.follow = task.id

      elseif effect == "ShowState" then
        local e = entities[task.id]
        if e and e[a] ~= nil then
          table.insert(uiDrawList, {label = a, value = e[a]})
        end
      else
        local effect_key = effect:lower()
        local resolved, handlerImpl = resolve(task, entityRegions, effect_key)
        if handlerImpl then
          handlerImpl(task.id, resolved, dt)
        else
          local impl = effect_impls[effect_key]
          if impl then
            impl(task.id, resolved, dt)
          else
            response = resolved
          end
        end
      end

      ok, effect, a, b = coroutine.resume(task.co, response)
      if not ok then
        print("Script error in " .. task.id .. ": " .. tostring(effect))
        break
      end
    end
  end

  $animUpdateLoop

  handleTriggers(dt)

  if camera.follow then
    local followed = entities[camera.follow]
    if followed and followed.x > camera.threshold then
      camera.x = followed.x - camera.threshold
    end
  end

  -- Cleanup
  for i = #tasks, 1, -1 do
    if tasks[i].dead then table.remove(tasks, i) end
  end
end

function love.draw()
  for _, region in ipairs(regions) do
    if region.hasColor and regionActive(region) then
      love.graphics.setColor(region.r, region.g, region.b, 1)
      love.graphics.rectangle("fill", region.x - camera.x, region.y - camera.y, region.w, region.h)
      love.graphics.setColor(1.0, 1.0, 1.0, 1.0)
    end
  end

  for name, e in pairs(entities) do
    if e then
      -- Animated spritesheet draw
      if e.sheet and e.quads and e.currentAnim then
        local anim = clove_findAnim(e.anims, e.currentAnim)
        local frameIdx = anim.frames[e.animFrame]
        local quad = e.quads[frameIdx]
        if quad then
          love.graphics.setColor(1, 1, 1, 1)

          -- Translate to draw position so scale doesnt
          -- shift frames that quad x/y offset is non-zero
          love.graphics.push()
          love.graphics.translate(e.x - camera.x, e.y - camera.y)
          love.graphics.draw(e.sheet, quad, 0, 0, 0,
            e.width / e.frameWidth, e.height / e.frameHeight)
          love.graphics.pop()
        end
      
      -- Static sprite
      elseif e.sprite then
        love.graphics.setColor(1, 1, 1, 1)
        love.graphics.draw(e.sprite, e.x - camera.x, e.y - camera.y, 0,
          e.width / e.sprite:getWidth(), e.height / e.sprite:getHeight())
      else
        love.graphics.setColor(1.0, 1.0, 1.0, 1.0)
        love.graphics.rectangle("fill", e.x - camera.x, e.y - camera.y, e.width, e.height)
      end
    end
  end

  love.graphics.setColor(1, 1, 1, 1)
  for i, item in ipairs(uiDrawList) do
    love.graphics.print(item.label .. ": " .. tostring(item.value), 10, 10 + (i - 1) * 20)
  end
end""".stripMargin

  def writeToFile(world: World, path: os.Path): Unit =
    val lua = wrap(world)
    os.makeDir.all(path / os.up)
    os.write.over(path, lua)

    // copy assets if they exist
    val assetsSource = os.pwd / "src" / "main" / "resources" / "assets"
    val assetsDest = path / os.up / "assets"
    if os.exists(assetsSource) then
      os.copy.over(assetsSource, assetsDest, createFolders = true)