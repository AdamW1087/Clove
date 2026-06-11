package clove.compiler

import clove.ast.*

object LuaEmitter:

  // Expression emission
  // inCondition (true when emitting inside animRule/region conditions)
  def emitExpr(expr: Expr, inCondition: Boolean = false): String = expr match
    case Expr.Num(v)       => v.toString
    case Expr.Str(v)       => s"\"$v\""
    case Expr.Bool(v)      => v.toString
    case Expr.Var(name)    => name
    case Expr.Not(e)       => s"not (${emitExpr(e, inCondition)})"
    case Expr.Negate(e)    => s"(-(${emitExpr(e, inCondition)}))"
    case Expr.Ceil(e)      => s"math.ceil(${emitExpr(e, inCondition)})"
    case Expr.Floor(e)     => s"math.floor(${emitExpr(e, inCondition)})"
    case Expr.Max(exprs*)  => s"math.max(${exprs.map(emitExpr(_, inCondition)).mkString(", ")})"
    case Expr.Min(exprs*)  => s"math.min(${exprs.map(emitExpr(_, inCondition)).mkString(", ")})"
    case Expr.KeyDown(key) => s"love.keyboard.isDown(\"$key\")"
    case Expr.JustPressed(key) => s"(_justPressed[\"$key\"] == true)"

    case Expr.StateRead(key) =>
      if inCondition then s"e[\"$key\"]"
      else sys.error(s"obsState(\"$key\") used outside animRule condition, use getState instead")

    case Expr.GlobalRead(key) =>
      if inCondition then s"globals[\"$key\"]"
      else sys.error(s"obsGlobal(\"$key\") used outside region/animRule condition, use getGlobal instead")

    case Expr.EntityRead(id, key) =>
      s"(entities[\"$id\"] and entities[\"$id\"][\"$key\"] or 0.0)"

    case Expr.EntityExists(id) =>
      s"(entities[\"$id\"] ~= nil)"

    case Expr.DeltaTime =>
      "_clove_dt"

    case Expr.Propagate =>
      s"{value = 1.0, propagate = true, op = \"*\"}"

    case Expr.BinOp(op, l, Expr.Propagate) =>
      s"{value = ${emitExpr(l)}, propagate = true, op = \"$op\", leftVal = true}"

    case Expr.BinOp(op, Expr.Propagate, r) =>
      s"{value = ${emitExpr(r)}, propagate = true, op = \"$op\", leftVal = false}"

    case Expr.BinOp(op, l, r) =>
      s"(${emitExpr(l, inCondition)} $op ${emitExpr(r, inCondition)})"

  def emitExprAsString(expr: Expr): String = expr match
    case Expr.Var(name) => s"\"$name\""
    case other          => emitExpr(other)

  // Handler fields as Lua table entries
  def emitHandlerFields(handles: Iterable[(String, Expr)], impls: Iterable[(String, Impl)]): String =
    val values = handles.map { (k, v) =>
      s"${k.toLowerCase} = ${emitExpr(v)}"
    }.mkString(", ")
    val implFields = impls.map { (k, impl) =>
      s"${k.toLowerCase}_impl = ${emitImpl(impl)}"
    }.mkString(", ")
    List(values, implFields).filter(_.nonEmpty).mkString(", ")

  // Statement emission
  def emitStatement(stmt: Statement, indent: Int): String =
    val pad = "  " * indent
    stmt match
      case Bind(varName, effect) =>
        s"${pad}local $varName = ${emitYield(effect)}"

      case Perform(effect) =>
        s"${pad}${emitYield(effect)}"

      case Discard(effect) =>
        s"${pad}${emitYield(effect)}"

      case Configure(c) =>
        sys.error(s"Configure($c) found in update script, should have been caught by validator")

      case If(cond, thenBranch) =>
        s"""${pad}if ${emitExpr(cond)} then
           |${emitScript(thenBranch, indent + 1)}
           |${pad}end""".stripMargin

      case IfElse(cond, thenBranch, elseBranch) =>
        s"""${pad}if ${emitExpr(cond)} then
           |${emitScript(thenBranch, indent + 1)}
           |${pad}else
           |${emitScript(elseBranch, indent + 1)}
           |${pad}end""".stripMargin

      case HandleWith(handler, body) =>
        val allFields = emitHandlerFields(handler.handles, handler.impls)
        val nameComment = handler.name.map(n => s" -- $n").getOrElse("")
        val bodyLua = emitScript(body, indent)
        if bodyLua.nonEmpty then
          s"""${pad}coroutine.yield("pushhandler", {$allFields})$nameComment
             |$bodyLua
             |${pad}coroutine.yield("pophandler")""".stripMargin
        else
          s"""${pad}coroutine.yield("pushhandler", {$allFields})$nameComment"""

  // Emit a coroutine.yield call. effect name followed by args
  private def yieldCall(name: String, args: String*): String =
    if args.isEmpty then s"coroutine.yield(\"$name\")"
    else s"coroutine.yield(\"$name\", ${args.mkString(", ")})"

  // Wrap a string to Lua string literal
  private def quote(s: String): String = "\"" + s + "\""

  // Emit coroutine.yield("name", { ...fields... })
  private def yieldTable(name: String, fields: String): String =
    s"""coroutine.yield("$name", {$fields})"""

  // Emit an (r, g, b) colour tuple as Lua fields
  private def rgbFields(c: (Double, Double, Double), rName: String, gName: String, bName: String): String =
    s"$rName=${c._1}, $gName=${c._2}, $bName=${c._3}"

  def emitYield(effect: Effect[?]): String = effect match
    case Effect.Move(dx, dy)                 => yieldCall("move", emitExpr(dx), emitExpr(dy))
    case Effect.Jump()                       => yieldCall("jump")
    case Effect.PlaySound(path)              => yieldCall("playsound", quote(path))
    case Effect.Gravity()                    => yieldCall("gravity")
    case Effect.Despawn()                    => yieldCall("despawn")
    case Effect.SetState(key, v)             => yieldCall("setstate", quote(key), emitExpr(v))
    case Effect.GetState(key)                => yieldCall("getstate", quote(key))
    case Effect.SetGlobal(key, v)            => yieldCall("setglobal", quote(key), emitExpr(v))
    case Effect.GetGlobal(key)               => yieldCall("getglobal", quote(key))
    case Effect.SetStateOf(targetId, key, v) => yieldCall("setstateof", quote(targetId), quote(key), emitExpr(v))
    case Effect.GetStateOf(targetId, key)    => yieldCall("getstateof", quote(targetId), quote(key))
    case Effect.Collides(target)             => yieldCall("collides", emitExprAsString(target))
    case Effect.Camera()                     => yieldCall("camera")
    case Effect.SetCamera(target)            => yieldCall("setcamera", quote(target))
    case Effect.Music()                      => yieldCall("music")
    case Effect.UserEffect(name)             => yieldCall(name)
    case Effect.SetSize(w, h)                => yieldCall("setsize", emitExpr(w), emitExpr(h))
    case Effect.SpawnAt(tpl, x, y)           => yieldCall("spawnat", quote(tpl), emitExpr(x), emitExpr(y))

    case _: Continuation[?] =>
      sys.error("resumeRead()/resumeWrite() can only be used inside a state handler impl (onGet/onSet)")

    // UI effects
    case UI.Bar(x, y, w, h, value, max, fg, bg) =>
      yieldTable("uibar",
        s"x=$x, y=$y, w=$w, h=$h, value=${emitExpr(value)}, max=${emitExpr(max)}, " +
        s"${rgbFields(fg, "r", "g", "b")}, ${rgbFields(bg, "rb", "gb", "bb")}")

    case UI.Label(x, y, prefix, value, fg) =>
      val valStr = value.map(v => s", value=${emitExpr(v)}").getOrElse("")
      yieldTable("uilabel",
        s"""x=$x, y=$y, prefix="$prefix"$valStr, ${rgbFields(fg, "r", "g", "b")}""")

    case UI.Sprites(x, y, image, count, spacing, w, h) =>
      yieldTable("uisprites",
        s"""x=$x, y=$y, image="$image", count=${emitExpr(count)}, spacing=$spacing, w=$w, h=$h""")

    case UI.Slots(x, y, size, images, selected, spacing) =>
      val imgList = images.map(quote).mkString(", ")
      yieldTable("uislots",
        s"x=$x, y=$y, size=$size, images={$imgList}, selected=${emitExpr(selected)}, spacing=$spacing")

    case UI.Image(x, y, w, h, image, fg) =>
      yieldTable("uiimage",
        s"""x=$x, y=$y, w=$w, h=$h, image="$image", ${rgbFields(fg, "r", "g", "b")}""")

  def emitScript(script: Script, indent: Int = 0): String =
    script.statements
      .map(emitStatement(_, indent))
      .filter(_.nonEmpty)
      .mkString("\n")

  def emitCoroutine(entityId: String, script: Script): String =
    s"""coroutine.create(function()
       |  -- script for $entityId
       |  local task_id = "$entityId"
       |  while true do
       |${emitScript(script, indent = 2)}
       |    coroutine.yield()
       |  end
       |end)""".stripMargin

  def emitInitCoroutine(entityId: String, script: Script): String =
    s"""coroutine.create(function()
       |  -- init script for $entityId
       |  local task_id = "$entityId"
       |${emitScript(script, indent = 2)}
       |end)""".stripMargin

  // Direct mode emission (for custom effects, handler impls)
  def emitDirectStatement(stmt: Statement, indent: Int): String =
    val pad = "  " * indent
    stmt match
      case Bind(varName, Effect.GetState(key)) =>
        s"${pad}local $varName = entities[task_id] and entities[task_id][\"$key\"]"
      case Bind(varName, Effect.GetGlobal(key)) =>
        s"${pad}local $varName = globals[\"$key\"]"

      case Perform(Effect.SetState(key, v)) =>
        s"${pad}if entities[task_id] then entities[task_id][\"$key\"] = ${emitExpr(v)} end"
      case Perform(Effect.SetGlobal(key, v)) =>
        s"${pad}globals[\"$key\"] = ${emitExpr(v)}"
      case Perform(Effect.ResumeWrite()) =>
        s"${pad}if entities[task_id] then entities[task_id][key] = value end"
      case Perform(Effect.ResumeRead()) =>
        s"${pad}return entities[task_id] and entities[task_id][key]"
      case Perform(Effect.ResumeWith(v)) =>
        s"${pad}return ${emitExpr(v)}"

      case Perform(Effect.SetSize(w, h)) =>
        s"${pad}if entities[task_id] then entities[task_id][\"width\"] = ${emitExpr(w)}; entities[task_id][\"height\"] = ${emitExpr(h)} end"

      case If(cond, thenBranch) =>
        s"""${pad}if ${emitExpr(cond)} then
           |${emitDirectScript(thenBranch, indent + 1)}
           |${pad}end""".stripMargin

      case IfElse(cond, thenBranch, elseBranch) =>
        s"""${pad}if ${emitExpr(cond)} then
           |${emitDirectScript(thenBranch, indent + 1)}
           |${pad}else
           |${emitDirectScript(elseBranch, indent + 1)}
           |${pad}end""".stripMargin

      case other =>
        sys.error(s"Unsupported statement in direct mode (handler impl / custom effect): $other")

  def emitDirectScript(script: Script, indent: Int = 0): String =
    script.statements
      .map(emitDirectStatement(_, indent))
      .filter(_.nonEmpty)
      .mkString("\n")

  // Shared impl emitter
  def emitImpl(impl: Impl): String =
    val binding =
      if impl.payload.isEmpty then ""
      else s"  local ${impl.payload.mkString(", ")} = ...\n"
    s"""function(task_id, resolved, dt, ...)
       |$binding${emitDirectScript(impl.body, indent = 1)}
       |end""".stripMargin

  // Spawn script emission
  def emitSpawnScript(entityId: String, script: Script): String =
    script.statements.map {
      case Perform(Effect.SetState(key, value)) =>
        s"  entities[\"$entityId\"][\"$key\"] = ${emitExpr(value)}"

      case Perform(Effect.SetSize(w, h)) =>
        s"""  entities["$entityId"]["width"] = ${emitExpr(w)}
           |  entities["$entityId"]["height"] = ${emitExpr(h)}""".stripMargin

      case Configure(SpawnConfig.SetSprite(path)) =>
        s"  entities[\"$entityId\"][\"spritePath\"] = \"$path\""

      case Configure(SpawnConfig.SetSpritesheet(path, fw, fh)) =>
        s"""  entities["$entityId"]["sheetPath"] = "$path"
           |  entities["$entityId"]["frameWidth"] = $fw
           |  entities["$entityId"]["frameHeight"] = $fh
           |  entities["$entityId"]["anims"] = {}
           |  entities["$entityId"]["currentAnim"] = nil
           |  entities["$entityId"]["animFrame"] = 1
           |  entities["$entityId"]["animTimer"] = 0""".stripMargin

      case Configure(SpawnConfig.AnimRule(name, frames, fps, condition, flipped)) =>
        val luaFrames = frames.map(_ + 1).mkString(", ")
        val condFn = condition match
          case None       => "function(e) return true end"
          case Some(expr) => s"function(e) return ${emitExpr(expr, inCondition = true)} end"
        s"""  table.insert(entities["$entityId"]["anims"], {name = "$name", frames = {$luaFrames}, fps = $fps, condition = $condFn, flipped = $flipped})"""

      case _ => ""
    }.filter(_.nonEmpty).mkString("\n")