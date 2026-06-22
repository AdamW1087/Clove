package clove.compiler.runtime

import clove.ast.*
import clove.dsl.{Region, Behaviour, Visual, VisualMode, World}
import clove.compiler.{LuaEmitter, WorldAnalyser}
import clove.compiler.WorldFeatures

object LoveRuntime:

  // The region table
  private def emitRegionTable(world: World): String =
    world.regions.zipWithIndex.map { (r, idx) =>
      val idStr = s"\"${r.id}\""

      val condStr = r.condition match
        case None       => "nil"
        case Some(expr) => s"function(globals) return ${LuaEmitter.emitExpr(expr, inCondition = true)} end"

      val (cr, cg, cb) = r.colour.getOrElse((1.0, 1.0, 1.0))

      val visualFields = r.visual match
        case None => "hasVisual = false"
        case Some(Visual(path, mode, tileSize)) =>
          val modeStr = mode match
            case VisualMode.Stretch => "\"stretch\""
            case VisualMode.Tile    => "\"tile\""
            case VisualMode.Sprite  => "\"sprite\""
          val tileSizeStr = tileSize match
            case None         => "nil"
            case Some((w, h)) => s"{w = $w, h = $h}"
          s"hasVisual = true, visualImage = \"$path\", visualMode = $modeStr, visualTileSize = $tileSizeStr"

      // Common fields shared across all region types
      val commonFields =
        s"id = $idStr, x = ${r.x}, y = ${r.y}, w = ${r.w}, h = ${r.h}, " +
        s"hasColor = ${r.colour.isDefined}, r = $cr, g = $cg, b = $cb, condition = $condStr, $visualFields"

      r.behaviour match
        case Behaviour.Basic(handlers) =>
          val hasHandlers = handlers.exists(h => h.handles.nonEmpty || h.impls.nonEmpty)
          val handlerTables = handlers
            .filter(h => h.handles.nonEmpty || h.impls.nonEmpty)
            .map(h => s"{${LuaEmitter.emitHandlerFields(h.handles, h.impls)}}")
            .mkString(", ")
          val handlersField = if handlerTables.nonEmpty then s", handlers = {$handlerTables}" else ""
          val typeFields = s"type = \"basic\", hasHandlers = $hasHandlers$handlersField"
          s"  {$commonFields, $typeFields}"

        case Behaviour.Solid(oneWay) =>
          s"  {$commonFields, type = \"solid\", oneWay = $oneWay}"

    }.mkString(",\n")

  private def emitGlobalsTable(world: World): String =
    world.initialGlobals.map { (k, v) =>
      s"  [\"$k\"] = ${LuaEmitter.emitExpr(v)}"
    }.mkString(",\n")

  private def emitDefaultHandlerTable(world: World): String =
    world.defaultHandlers.map { h =>
      val values = h.handles.map { (k, v) => s"  ${k.toLowerCase} = ${LuaEmitter.emitExpr(v)}" }
      val impls = h.impls.map { (k, f) => s"  ${k.toLowerCase}_impl = ${LuaEmitter.emitImpl(f)}" }
      (values ++ impls).mkString(",\n")
    }.mkString(",\n")

  private def emitEffectImpls(world: World): String =
    world.customEffects.map { e =>
      val body = e.impl(Expr.Var("resolved"), Expr.Var("dt"))
      s"  ${e.name.toLowerCase} = ${LuaEmitter.emitImpl(Impl(body))}"
    }.mkString(",\n")

  // Template definitions, init/update scripts, and the runtime spawn helper,
  private def emitTemplateSpawnScripts(world: World): String =
    if world.templates.isEmpty then "" else
      val defs = world.templates.values.map { e =>
        s"""  ["${e.name}"] = function(id)\n${LuaEmitter.emitSpawnScript(e.name, e.spawnScript).replace(s""""${e.name}"""", "id")}\n  end"""
      }.mkString(",\n")

      val inits = world.templates.values
        .filter(_.initScript.statements.nonEmpty)
        .map { e =>
          s"""  ["${e.name}"] = function(id) return ${LuaEmitter.emitInitCoroutine(e.name, e.initScript).replace(s""""${e.name}"""", "id")} end"""
        }.mkString(",\n")

      val scripts = world.templates.values
        .filter(_.updateScript.statements.nonEmpty)
        .map { e =>
          s"""  ["${e.name}"] = function(id) return ${LuaEmitter.emitCoroutine(e.name, e.updateScript).replace(s""""${e.name}"""", "id")} end"""
        }.mkString(",\n")

      s"""local templateDefs = {
         |$defs
         |}
         |local templateInitScripts = {
         |$inits
         |}
         |local templateScripts = {
         |$scripts
         |}
         |local templateCounts = {}
         |
         |-- Spawn a template at runtime, inserts an init and/or update script
         |local function handleSpawnAt(name, x, y)
         |  local tpl = templateDefs[name]
         |  if not tpl then return end
         |  templateCounts[name] = (templateCounts[name] or 0) + 1
         |  local newId = name .. "_" .. templateCounts[name]
         |  entities[newId] = {vy = 0, x = x, y = y}
         |  tpl(newId)
         |  if entities[newId].spritePath then
         |    entities[newId].sprite = love.graphics.newImage(entities[newId].spritePath)
         |  end
         |  local updateFn = templateScripts[name]
         |  local initFn   = templateInitScripts[name]
         |  if initFn then
         |    table.insert(tasks, {id = newId, co = initFn(newId), handlerStack = {}, pendingUpdate = updateFn})
         |  elseif updateFn then
         |    table.insert(tasks, {id = newId, co = updateFn(newId), handlerStack = {}})
         |  end
         |end""".stripMargin

  // Per entity update coroutines
  private def emitCoroutines(world: World): String =
    "local _scripts = {}\n" +
    world.entities
      .filter(_.updateScript.statements.nonEmpty)
      .map { e => s"""_scripts["${e.name}"] = ${LuaEmitter.emitCoroutine(e.name, e.updateScript)}""" }
      .mkString("\n")

  // Per entity init coroutines
  private def emitInitCoroutines(world: World): String =
    "local _initScripts = {}\n" +
    world.entities
      .filter(_.initScript.statements.nonEmpty)
      .map { e => s"""_initScripts["${e.name}"] = ${LuaEmitter.emitInitCoroutine(e.name, e.initScript)}""" }
      .mkString("\n")

  private def emitSpawnSetup(world: World): String =
    world.entities.map { e =>
      s"""  entities["${e.name}"] = {vy = 0}
         |${LuaEmitter.emitSpawnScript(e.name, e.spawnScript)}""".stripMargin
    }.mkString("\n")

  private def emitTaskSetup(world: World): String =
    world.entities.map { e =>
      val hasInit = e.initScript.statements.nonEmpty
      val hasUpdate = e.updateScript.statements.nonEmpty
      (hasInit, hasUpdate) match
        case (true, true) =>
          s"""  table.insert(tasks, {id = "${e.name}", co = _initScripts["${e.name}"], handlerStack = {}, pendingUpdate = function(id) return _scripts["${e.name}"] end})"""
        case (true, false) =>
          s"""  table.insert(tasks, {id = "${e.name}", co = _initScripts["${e.name}"], handlerStack = {}, pendingUpdate = nil})"""
        case (false, true) =>
          s"""  table.insert(tasks, {id = "${e.name}", co = _scripts["${e.name}"], handlerStack = {}})"""
        case (false, false) =>
          ""
    }.filter(_.nonEmpty).mkString("\n")

  private def collectUIImagePaths(script: Script): List[String] =
    ScriptTraversal.collectStatements(script) {
      case Perform(UI.Sprites(_, _, image, _, _, _, _)) => List(image)
      case Perform(UI.Slots(_, _, _, images, _, _))     => images
      case Perform(UI.Image(_, _, _, _, image, _))      => List(image)
      case _                                            => Nil
    }

  // Image cache loader for region visuals and UI images
  private def emitVisualCacheLoad(world: World, features: WorldFeatures): String =
    val visualImagePaths = world.regions.flatMap(_.visual).map(_.path).distinct
    val uiImagePaths     = world.entities.flatMap(e => collectUIImagePaths(e.updateScript)).distinct
    val allImagePaths    = (visualImagePaths ++ uiImagePaths).distinct

    if features.usesVisuals || features.usesUI then
      val cacheEntries = allImagePaths.map { path =>
        s"  images[\"$path\"] = love.graphics.newImage(\"$path\")"
      }.mkString("\n")
      s"  -- Image cache (region visuals + UI)\n$cacheEntries"
    else ""

  private def emitHotReloadBlock(hotReload: Boolean): String =
    if hotReload then
      """|local _lastModified = love.filesystem.getInfo("main.lua") and love.filesystem.getInfo("main.lua").modtime or 0
         |""".stripMargin
    else ""

  private def emitHotReloadCheck(hotReload: Boolean): String =
    if hotReload then
      """|  local info = love.filesystem.getInfo("main.lua")
         |  if info and info.modtime ~= _lastModified then love.event.quit("restart") end
         |""".stripMargin
    else ""

  // Music state declarations
  private def emitMusicStateDecl(features: WorldFeatures): String =
    if features.usesMusic then
      "local MUSIC_FADE = 1.0\nlocal _currentTrack = nil\nlocal _musicCurrent = nil\nlocal _musicPrevious = nil\nlocal _fadeProgress = 0"
    else ""

  // Cached player
  private def emitPlaySoundFn(features: WorldFeatures): String =
    if features.usesSound then
      """|local function playSound(path)
         |  local src = _sounds[path]
         |  if not src then
         |    src = love.audio.newSource(path, "static")
         |    _sounds[path] = src
         |  end
         |  src:clone():play()
         |end""".stripMargin
    else ""

  // Edge triggered key tracking
  private def emitKeypressedFn(features: WorldFeatures): String =
    if features.usesJustPressed then
      """|function love.keypressed(key)
         |  _justPressed[key] = true
         |end""".stripMargin
    else ""

  // Per frame, per entity region resolution
  private def emitEntityRegionsSetup(features: WorldFeatures): String =
    if features.usesHandlers then
      """|  local entityRegions = {}
         |  for id, e in pairs(entities) do
         |    entityRegions[id] = getHandledRegions(e)
         |  end""".stripMargin
    else "  local entityRegions = {}"

  // Camera follow
  private def emitCameraFollow(features: WorldFeatures): String =
    if features.usesCamera then
      """|  if camera.target then
         |    local followed = entities[camera.target]
         |    if followed then
         |      -- target centre (world)
         |      local tx = followed.x + (followed.width or 0) / 2
         |      local ty = followed.y + (followed.height or 0) / 2
         |      if not camera.initialised then
         |        -- snap straight to the target the first frame we follow it
         |        camera.x = tx
         |        camera.y = ty
         |        camera.initialised = true
         |      else
         |        -- deadzone half-width in world units
         |        local hx = (camera.dzx / 2) / camera.zoom
         |        local hy = (camera.dzy / 2) / camera.zoom
         |        local dx = tx - camera.x
         |        if dx >  hx then camera.x = camera.x + (dx - hx) end
         |        if dx < -hx then camera.x = camera.x + (dx + hx) end
         |        local dy = ty - camera.y
         |        if dy >  hy then camera.y = camera.y + (dy - hy) end
         |        if dy < -hy then camera.y = camera.y + (dy + hy) end
         |      end
         |    end
         |  end""".stripMargin
    else ""


  def wrap(world: World, hotReload: Boolean = false): String =
    val features = WorldAnalyser.analyse(world)

    val regionTable          = emitRegionTable(world)
    val globalsTable         = emitGlobalsTable(world)
    val defaultHandlerTable  = emitDefaultHandlerTable(world)
    val effectImpls          = emitEffectImpls(world)
    val templateSpawnScripts = emitTemplateSpawnScripts(world)
    val coroutines           = emitCoroutines(world)
    val initCoroutines       = emitInitCoroutines(world)
    val spawnSetup           = emitSpawnSetup(world)
    val taskSetup            = emitTaskSetup(world)
    val visualCacheLoad      = emitVisualCacheLoad(world, features)
    val hotReloadBlock       = emitHotReloadBlock(hotReload)
    val hotReloadCheck       = emitHotReloadCheck(hotReload)

    val cx = "0"
    val cy = "0"

    s"""-- Generated by Clove
$hotReloadBlock
local entities = {}
local tasks    = {}
${if features.usesCamera  then "local camera = {x = 0, y = 0, target = nil, zoom = 1.0, dzx = 0.0, dzy = 0.0}" else ""}

${if features.usesGlobals then s"local globals = {\n$globalsTable\n}" else ""}

local defaultHandlers = {
$defaultHandlerTable
}

local regions = {
$regionTable
}

local _clove_dt = nil

${if features.usesJustPressed then "local _justPressed = {}" else ""}
${if features.usesSound then "local _sounds = {}" else ""}
${emitMusicStateDecl(features)}
${UIRuntime.drawListDecl(features)}
${if features.usesVisuals || features.usesUI then "local images = {}" else ""}

$templateSpawnScripts


${LuaRuntime.utilityFunctions(features)}

${LuaRuntime.resolveFunction}

${emitPlaySoundFn(features)}

${emitKeypressedFn(features)}

${LuaRuntime.resolveDispatchFunction}

${LuaRuntime.builtinHandlers(features)}

${LuaRuntime.setSize}

${if features.usesMusic then LuaRuntime.musicUpdate else ""}

${LuaRuntime.dispatchTable(features)}

${if features.usesVisuals then LuaRuntime.visualDrawFn else ""}

local effect_impls = {
$effectImpls
}

$coroutines
$initCoroutines

function love.load()
love.window.setMode(1280, 720)
love.graphics.setDefaultFilter("nearest", "nearest", 1)
love.graphics.setLineStyle("rough")
$spawnSetup
$taskSetup

  for name, e in pairs(entities) do
    if e.spritePath then
      e.sprite = love.graphics.newImage(e.spritePath)
    end
${if features.usesAnimations then LuaRuntime.spritesheetLoad else ""}
  end

$visualCacheLoad
end

function love.update(dt)
_clove_dt = dt
$hotReloadCheck
${UIRuntime.drawListClear(features)}
${emitEntityRegionsSetup(features)}

  for _, task in ipairs(tasks) do
    task.handlerStack = {}
    local ok, effect, a, b, c = coroutine.resume(task.co)

    while effect ~= nil do
      local response = nil

      if effect == "pushhandler" then
        table.insert(task.handlerStack, a)

      elseif effect == "pophandler" then
        table.remove(task.handlerStack)

      elseif effect == "despawn" then
        entities[task.id] = nil
        task.dead = true
        break

      elseif effect == "abortframe" then
        local aok, anext = coroutine.resume(task.co, nil)
        while anext ~= nil do
          aok, anext = coroutine.resume(task.co, nil)
        end
        break

      else
        local builtin = dispatch[effect]
        if builtin then
          response = builtin(task, entityRegions, a, b, c, dt)
        else
          local effect_key = effect:lower()
          local resolved, handlerImpl = resolve(task, entityRegions, effect_key)
          if handlerImpl then
            response = handlerImpl(task.id, resolved, dt, a, b, c)
          else
            local impl = effect_impls[effect_key]
            if impl then response = impl(task.id, resolved, dt, a, b, c)
            else response = resolved
            end
          end
        end
      end

      ok, effect, a, b, c = coroutine.resume(task.co, response)
      if not ok then
        print("Script error in " .. task.id .. ": " .. tostring(effect))
        break
      end
    end

    -- Stopping onInit coroutines from continuing
    if effect == nil and coroutine.status(task.co) == "dead" and not task.dead then
      if task.pendingUpdate then task.dead = true end
    end
  end

${if features.usesAnimations then LuaRuntime.animUpdate else ""}
${emitCameraFollow(features)}

  for i = #tasks, 1, -1 do
    if tasks[i].dead then
      if tasks[i].pendingUpdate then
        local t = tasks[i]
        table.insert(tasks, {id = t.id, co = t.pendingUpdate(t.id), handlerStack = {}})
      end
      table.remove(tasks, i)
    end
  end
${if features.usesJustPressed then "  _justPressed = {}" else ""}
${if features.usesMusic then "  updateMusic(dt)" else ""}
end

function love.draw()
${if features.usesCamera then
  """|  love.graphics.push()
     |  local _sw, _sh = love.graphics.getWidth(), love.graphics.getHeight()
     |  love.graphics.translate(_sw / 2, _sh / 2)
     |  love.graphics.scale(camera.zoom, camera.zoom)
     |  love.graphics.translate(-camera.x, -camera.y)""".stripMargin
  else ""}
  for _, region in ipairs(regions) do
    if regionActive(region) then
      if region.hasColor then
        love.graphics.setColor(region.r, region.g, region.b, 1)
        love.graphics.rectangle("fill", region.x - $cx, region.y - $cy, region.w, region.h)
      end
${if features.usesVisuals then
    """|      if region.hasVisual then
       |        if not region.hasColor then love.graphics.setColor(1, 1, 1, 1) end
       |        drawRegionVisual(region)
       |      end""".stripMargin
  else ""}
      love.graphics.setColor(1, 1, 1, 1)
    end
  end

  for name, e in pairs(entities) do
    if e then
${if features.usesAnimations then
   s"""|${LuaRuntime.animDraw}
      elseif e.sprite then""".stripMargin
  else
    "      if e.sprite then"}
        love.graphics.setColor(1, 1, 1, 1)
        love.graphics.draw(e.sprite, e.x - $cx, e.y - $cy, 0,
          e.width / e.sprite:getWidth(), e.height / e.sprite:getHeight())
      else
        love.graphics.setColor(1, 1, 1, 1)
        love.graphics.rectangle("fill", e.x - $cx, e.y - $cy, e.width, e.height)
      end
    end
  end
${if features.usesCamera then "  love.graphics.pop()" else ""}

${UIRuntime.renderBlock(features)}
end""".stripMargin

  def writeToFile(world: World, path: os.Path, hotReload: Boolean = false): Unit =
    val lua = wrap(world, hotReload)
    os.makeDir.all(path / os.up)
    os.write.over(path, lua)
    val assetsSource = os.pwd / "src" / "main" / "resources" / "assets"
    val assetsDest = path / os.up / "assets"
    if os.exists(assetsSource) then
      os.copy.over(assetsSource, assetsDest, createFolders = true)