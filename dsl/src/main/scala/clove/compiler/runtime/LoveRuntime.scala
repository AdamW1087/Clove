package clove.compiler.runtime

import clove.ast.*
import clove.dsl.{Region, Behaviour, Visual, VisualMode, World}
import clove.compiler.{LuaEmitter, WorldAnalyser}

object LoveRuntime:

  val groundLevel = 600

  def wrap(world: World, hotReload: Boolean = false): String =
    val features = WorldAnalyser.analyse(world)

    val visualImagePaths = world.regions
      .flatMap(_.visual)
      .map(_.path)
      .distinct

    // Emission helpers
    val regionTable = world.regions.zipWithIndex.map { (r, idx) =>
      val idStr = r.id.map(id => s"\"$id\"").getOrElse(s"\"region_$idx\"")

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
          val overrides = handlers.flatMap(_.handles).map { (k, v) =>
            s"${k.toLowerCase} = ${LuaEmitter.emitExpr(v)}"
          }.mkString(", ")
          val implOverrides = handlers.flatMap(_.impls).map { (k, f) =>
            s"${k.toLowerCase}_impl = ${LuaEmitter.emitImpl(f)}"
          }.mkString(", ")
          val allFields = List(overrides, implOverrides).filter(_.nonEmpty).mkString(", ")
          val hasHandlers = handlers.exists(h => h.handles.nonEmpty || h.impls.nonEmpty)
          val typeFields = s"type = \"basic\", hasHandlers = $hasHandlers" +
                            (if allFields.nonEmpty then s", $allFields" else "")
          s"  {$commonFields, $typeFields}"

        case Behaviour.Solid(oneWay) =>
          s"  {$commonFields, type = \"solid\", oneWay = $oneWay}"

        case Behaviour.Trigger(onEnter, onExit) =>
          val enterLua = LuaEmitter.emitTriggerScript(onEnter)
          val exitLua  = LuaEmitter.emitTriggerScript(onExit)
          s"""  {$commonFields, type = "trigger",
             |   onEnter = function(task_id, globals) $enterLua end,
             |   onExit  = function(task_id, globals) $exitLua end}""".stripMargin

    }.mkString(",\n")

    val globalsTable = world.initialGlobals.map { (k, v) =>
      s"  [\"$k\"] = ${LuaEmitter.emitExpr(v)}"
    }.mkString(",\n")

    val defaultHandlerTable = world.defaultHandlers.map { h =>
      val values = h.handles.map { (k, v) => s"  ${k.toLowerCase} = ${LuaEmitter.emitExpr(v)}" }
      val impls = h.impls.map { (k, f) => s"  ${k.toLowerCase}_impl = ${LuaEmitter.emitImpl(f)}" }
      (values ++ impls).mkString(",\n")
    }.mkString(",\n")

    val effectImpls = world.customEffects.map { e =>
      val body = e.impl(Expr.Var("resolved"), Expr.Var("dt"))
      s"  ${e.name.toLowerCase} = ${LuaEmitter.emitImpl(Impl(body))}"
    }.mkString(",\n")

    val templateCoroutines = world.templates.values
      .filter(_.updateScript.statements.nonEmpty)
      .map { e => s"local ${e.name}Template = ${LuaEmitter.emitCoroutine(e.name, e.updateScript)}" }
      .mkString("\n")

    val templateSpawnScripts = if world.templates.isEmpty then "" else
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

    // Store all coroutines
    val coroutines =
      "local _scripts = {}\n" +
      world.entities
        .filter(_.updateScript.statements.nonEmpty)
        .map { e => s"""_scripts["${e.name}"] = ${LuaEmitter.emitCoroutine(e.name, e.updateScript)}""" }
        .mkString("\n")

    val initCoroutines =
      "local _initScripts = {}\n" +
      world.entities
        .filter(_.initScript.statements.nonEmpty)
        .map { e => s"""_initScripts["${e.name}"] = ${LuaEmitter.emitInitCoroutine(e.name, e.initScript)}""" }
        .mkString("\n")

    val spawnSetup = world.entities.map { e =>
      s"""  entities["${e.name}"] = {vy = 0}
         |${LuaEmitter.emitSpawnScript(e.name, e.spawnScript)}""".stripMargin
    }.mkString("\n")

    val taskSetup = world.entities.map { e =>
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

    def collectUIImagePaths(script: Script): List[String] =
      script.statements.flatMap {
        case Perform(UI.Sprites(_, _, image, _, _, _, _)) => List(image)
        case Perform(UI.Slots(_, _, _, images, _, _))     => images
        case Perform(UI.Image(_, _, _, _, image, _))      => List(image)
        case If(_, t)                                     => collectUIImagePaths(t)
        case IfElse(_, t, e)                              => collectUIImagePaths(t) ++ collectUIImagePaths(e)
        case HandleWith(_, body)                          => collectUIImagePaths(body)
        case _                                            => Nil
      }

    val uiImagePaths = world.entities
      .flatMap(e => collectUIImagePaths(e.updateScript))
      .distinct

    val allImagePaths = (visualImagePaths ++ uiImagePaths).distinct

    val visualCacheLoad = if features.usesVisuals || features.usesUI then
      val cacheEntries = allImagePaths.map { path =>
        s"  images[\"$path\"] = love.graphics.newImage(\"$path\")"
      }.mkString("\n")
      s"  -- Image cache (region visuals + UI)\n$cacheEntries"
    else ""

    val hotReloadBlock = if hotReload then
      """|local _lastModified = love.filesystem.getInfo("main.lua") and love.filesystem.getInfo("main.lua").modtime or 0
         |""".stripMargin
    else ""

    val hotReloadCheck = if hotReload then
      """|  local info = love.filesystem.getInfo("main.lua")
         |  if info and info.modtime ~= _lastModified then love.event.quit("restart") end
         |""".stripMargin
    else ""

    // Generated Lua
    val cx = if features.usesCamera then "camera.x" else "0"
    val cy = if features.usesCamera then "camera.y" else "0"

    s"""-- Generated by Clove
$hotReloadBlock
local entities = {}
local tasks    = {}
${if features.usesCamera  then "local camera = {x = 0, y = 0, follow = nil, threshold = 400}" else ""}
${if features.usesGravity then s"local GROUND = $groundLevel" else ""}

${if features.usesGlobals then s"local globals = {\n$globalsTable\n}" else ""}

local defaultHandlers = {
$defaultHandlerTable
}

local regions = {
$regionTable
}

_clove_dt = nil

${if features.usesJustPressed then "local _justPressed = {}" else ""}
${if features.usesSound then "local _sounds = {}" else ""}
${UIRuntime.drawListDecl(features)}
${if features.usesTriggers  then "local prevOverlap = {}" else ""}
${if features.usesVisuals || features.usesUI then "local images = {}" else ""}

$templateSpawnScripts


${LuaRuntime.utilityFunctions(features)}

${LuaRuntime.resolveFunction}

${if features.usesSound then
  """|-- Cache sources by path
     |local function playSound(path)
     |  local src = _sounds[path]
     |  if not src then
     |    src = love.audio.newSource(path, "static")
     |    _sounds[path] = src
     |  end
     |  src:clone():play()
     |end""".stripMargin
  else ""}

${if features.usesJustPressed then
  """|function love.keypressed(key)
     |  _justPressed[key] = true
     |end""".stripMargin
  else ""}

${LuaRuntime.resolveDispatchFunction}

${LuaRuntime.builtinHandlers(features)}

${LuaRuntime.setSize}

${LuaRuntime.dispatchTable(features)}

${if features.usesVisuals then LuaRuntime.visualDrawFn else ""}

local effect_impls = {
$effectImpls
}

$coroutines
$initCoroutines

function love.load()
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
${if features.usesHandlers then
    """|  local entityRegions = {}
       |  for id, e in pairs(entities) do
       |    entityRegions[id] = getHandledRegions(e)
       |  end""".stripMargin
  else "  local entityRegions = {}"}

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
${if features.usesTriggers   then "  handleTriggers()" else ""}
${if features.usesCamera then
    """|  if camera.follow then
       |    local followed = entities[camera.follow]
       |    if followed and followed.x > camera.threshold then
       |      camera.x = followed.x - camera.threshold
       |    end
       |  end""".stripMargin
  else ""}

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
end

function love.draw()
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