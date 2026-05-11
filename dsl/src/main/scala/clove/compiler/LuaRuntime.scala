package clove.compiler


object LuaRuntime:

  def utilityFunctions(f: WorldFeatures): String =
    val checkCollision = if f.usesCollides then
      """|local function checkCollision(a, b)
         |  if not a or not b then return false end
         |  return a.x < b.x + b.width and
         |         a.x + a.width > b.x and
         |         a.y < b.y + b.height and
         |         a.y + a.height > b.y
         |end
         |""".stripMargin
    else ""

    val findAnim = if f.usesAnimations then
      """|local function clove_findAnim(anims, name)
         |  for _, a in ipairs(anims) do
         |    if a.name == name then return a end
         |  end
         |  return anims[#anims]
         |end
         |""".stripMargin
    else ""

    val getHandledRegions = if f.usesHandlers then
      """|-- Returns effect-scope regions the entity is currently inside (used in resolve)
         |local function getHandledRegions(e)
         |  local result = {}
         |  for _, region in ipairs(regions) do
         |    if region.type == "basic" and region.hasHandlers and regionActive(region) and insideRegion(e, region) then
         |      table.insert(result, region)
         |    end
         |  end
         |  return result
         |end
         |""".stripMargin
    else ""

    s"""|$checkCollision
        |local function insideRegion(e, r)
        |  if not e then return false end
        |  return e.x < r.x + r.w and
        |         e.x + e.width > r.x and
        |         e.y < r.y + r.h and
        |         e.y + e.height > r.y
        |end
        |
        |local function regionActive(region)
        |  if region.condition == nil then return true end
        |  return region.condition(${if f.usesGlobals then "globals" else "{}"})
        |end
        |
        |$findAnim
        |$getHandledRegions""".stripMargin

  // Walks the handler stack, composing values via propagate()
  // Returns (resolvedValue, firstImplFound)
  val resolveFunction: String =
    """|local function resolve(task, entityRegions, key, i, foundImpl)
       |  local taskRegions = entityRegions[task.id] or {}
       |  local stackSize   = #task.handlerStack
       |  local regionSize  = #taskRegions
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
       |    if foundImpl == nil and entryImpl ~= nil then foundImpl = entryImpl end
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

  def builtinHandlers(f: WorldFeatures): String =
    val gravity = if f.usesGravity then
      """|local function handleGravity(task, entityRegions, dt)
         |  local resolved, impl = resolve(task, entityRegions, "gravity")
         |  local e = entities[task.id]
         |  if not e then return end
         |
         |  if impl then impl(task.id, resolved, dt); return end
         |
         |  if not resolved then return end
         |
         |  e.vy = (e.vy or 0) + resolved * dt
         |  e.y  = e.y + e.vy
         |
         |  -- Solid collision (vertical)
         |  for _, region in ipairs(regions) do
         |    if region.type == "solid" and regionActive(region) then
         |      if insideRegion(e, region) then
         |        if e.vy >= 0 and not region.oneWay then
         |          e.y = region.y - e.height; e.vy = 0; e.grounded = true
         |        elseif e.vy >= 0 and region.oneWay then
         |          local prevBottom = (e.y - e.vy * dt) + e.height
         |          if prevBottom <= region.y + 2 then
         |            e.y = region.y - e.height; e.vy = 0; e.grounded = true
         |          end
         |        elseif e.vy < 0 and not region.oneWay then
         |          e.y = region.y + region.h; e.vy = 0
         |        end
         |      end
         |    end
         |  end
         |
         |  -- Ground floor
         |  if e.y + e.height >= GROUND then
         |    e.y = GROUND - e.height; e.vy = 0; e.grounded = true
         |  elseif e.y <= 0 then
         |    e.y = 0; e.vy = 0
         |  else
         |    local onSolid = false
         |    for _, region in ipairs(regions) do
         |      if region.type == "solid" and regionActive(region) then
         |        if math.abs((e.y + e.height) - region.y) < 2 and
         |           e.x + e.width > region.x and e.x < region.x + region.w then
         |          onSolid = true; break
         |        end
         |      end
         |    end
         |    if not onSolid then e.grounded = false end
         |  end
         |end
         |""".stripMargin
    else ""

    val jump = if f.usesJump then
      """|local function handleJump(task, entityRegions, dt)
         |  local resolved, impl = resolve(task, entityRegions, "jump")
         |  if impl then impl(task.id, resolved, dt)
         |  else
         |    local e = entities[task.id]
         |    if e and e.grounded then e.vy = -resolved; e.grounded = false end
         |  end
         |end
         |""".stripMargin
    else ""

    val move = if f.usesMove then
      """|-- Resolves solid region collisions on a single axis after movement
         |-- axis: "x" or "y", delta: signed movement on that axis
         |local function resolveSolidCollision(e, axis, delta)
         |  for _, region in ipairs(regions) do
         |    if region.type == "solid" and not region.oneWay and regionActive(region) then
         |      if insideRegion(e, region) then
         |        if axis == "x" then
         |          if delta > 0 then e.x = region.x - e.width
         |          elseif delta < 0 then e.x = region.x + region.w end
         |        else
         |          if delta > 0 then e.y = region.y - e.height
         |          elseif delta < 0 then e.y = region.y + region.h end
         |        end
         |      end
         |    end
         |  end
         |end
         |
         |local function handleMove(task, entityRegions, a, b, dt)
         |  local e = entities[task.id]
         |  if not e then return end
         |
         |  local resolved, impl = resolve(task, entityRegions, "move")
         |
         |  if impl then impl(task.id, resolved, dt); return end
         |  local dx = a * dt
         |  local dy = b * dt
         |
         |  if resolved then dx = dx * resolved; dy = dy * resolved end
         |  e.x = e.x + dx; resolveSolidCollision(e, "x", dx)
         |  e.y = e.y + dy; resolveSolidCollision(e, "y", dy)
         |end
         |""".stripMargin
    else ""

    val collides = if f.usesCollides then
      """|local function handleCollides(task, targetID)
         |  local e = entities[task.id]
         |  local target = entities[targetID]
         |  if e and target then return checkCollision(e, target) end
         |  return false
         |end
         |""".stripMargin
    else ""

    val triggers = if f.usesTriggers then
      """|local function handleTriggers()
         |  for id, e in pairs(entities) do
         |    if not prevOverlap[id] then prevOverlap[id] = {} end
         |    for _, region in ipairs(regions) do
         |      if region.type == "trigger" and regionActive(region) then
         |        local rid       = region.id
         |        local isInside  = insideRegion(e, region)
         |        local wasInside = prevOverlap[id][rid] or false
         |        if isInside and not wasInside then region.onEnter(id, globals)
         |        elseif not isInside and wasInside then region.onExit(id, globals)
         |        end
         |        prevOverlap[id][rid] = isInside
         |      end
         |    end
         |  end
         |end
         |""".stripMargin
    else ""

    s"$gravity$jump$move$collides$triggers"

  val setSize: String =
    """|local function handleSetSize(task, a, b)
       |  local e = entities[task.id]
       |  if e then e.width = a; e.height = b end
       |end""".stripMargin

  def dispatchTable(f: WorldFeatures): String =
    val entries = List(
      if f.usesGravity  then Some("  Gravity  = function(task, er, a, b, dt) handleGravity(task, er, dt) end,") else None,
      if f.usesJump     then Some("  Jump     = function(task, er, a, b, dt) handleJump(task, er, dt) end,") else None,
      if f.usesMove     then Some("  Move     = function(task, er, a, b, dt) handleMove(task, er, a, b, dt) end,") else None,
      Some("  SetSize  = function(task, er, a, b, dt) handleSetSize(task, a, b) end,"),
      if f.usesCollides then Some("  Collides = function(task, er, a, b, dt) return handleCollides(task, a) end,") else None,
      Some("""|  SetState = function(task, er, a, b, dt)
              |    if entities[task.id] then entities[task.id][a] = b end
              |  end,
              |  GetState = function(task, er, a, b, dt)
              |    if entities[task.id] then return entities[task.id][a] end
              |  end,""".stripMargin),
      if f.usesGlobals   then Some("""|  SetGlobal = function(task, er, a, b, dt) globals[a] = b end,
                                      |  GetGlobal  = function(task, er, a, b, dt) return globals[a] end,""".stripMargin) else None,
      if f.usesCamera    then Some("  Camera    = function(task, er, a, b, dt) camera.follow = task.id end,") else None,
      if f.usesShowState then Some("""|  ShowState = function(task, er, a, b, dt)
                                      |    local e = entities[task.id]
                                      |    if e and e[a] ~= nil then table.insert(uiDrawList, {label = a, value = e[a]}) end
                                      |  end,""".stripMargin) else None,
    ).flatten.mkString("\n")

    s"""|local dispatch = {
        |$entries
        |}""".stripMargin

  val spritesheetLoad: String =
    """|    -- Spritesheet: load image and build quad table
       |    if e.sheetPath then
       |      e.sheet = love.graphics.newImage(e.sheetPath)
       |      local sheetW = e.sheet:getWidth()
       |      local sheetH = e.sheet:getHeight()
       |      local cols = math.floor(sheetW / e.frameWidth)
       |      local rows = math.floor(sheetH / e.frameHeight)
       |      e.quads = {}
       |      for row = 0, rows - 1 do
       |        for col = 0, cols - 1 do
       |          local idx = row * cols + col + 1
       |          e.quads[idx] = love.graphics.newQuad(
       |            col * e.frameWidth, row * e.frameHeight,
       |            e.frameWidth, e.frameHeight,
       |            sheetW, sheetH)
       |        end
       |      end
       |      -- Initialise animation state if not set by spawn script
       |      if not e.currentAnim and e.anims and #e.anims > 0 then
       |        e.currentAnim = e.anims[#e.anims].name
       |        e.animFrame   = 1
       |        e.animTimer   = 0
       |      end
       |    end""".stripMargin

  val animUpdate: String =
    """|  -- Animated sprite update
       |  for id, e in pairs(entities) do
       |    if e.anims and #e.anims > 0 then
       |      local activeName = nil
       |      for _, rule in ipairs(e.anims) do
       |        if rule.condition(e) then activeName = rule.name; break end
       |      end
       |      if activeName and activeName ~= e.currentAnim then
       |        e.currentAnim = activeName; e.animFrame = 1; e.animTimer = 0
       |      end
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

  val animDraw: String =
    """|      if e.sheet and e.quads and e.currentAnim then
       |        local anim     = clove_findAnim(e.anims, e.currentAnim)
       |        local frameIdx = anim.frames[e.animFrame]
       |        local flipX    = anim.flipped and -1 or 1
       |        local quad     = e.quads[frameIdx]
       |        if quad then
       |          local sx = (e.width / e.frameWidth) * flipX
       |          local ox = anim.flipped and e.frameWidth or 0
       |
       |          love.graphics.setColor(1, 1, 1, 1)
       |          love.graphics.push()
       |          love.graphics.translate(e.x - camera.x, e.y - camera.y)
       |          love.graphics.draw(e.sheet, quad, 0, 0, 0, sx, e.height / e.frameHeight, ox, 0)
       |          love.graphics.pop()
       |        end""".stripMargin