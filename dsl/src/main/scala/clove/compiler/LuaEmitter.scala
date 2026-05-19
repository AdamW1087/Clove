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
    case Expr.Max(exprs*)  => s"math.max(${exprs.map(emitExpr(_, inCondition)).mkString(", ")})"
    case Expr.Min(exprs*)  => s"math.min(${exprs.map(emitExpr(_, inCondition)).mkString(", ")})"
    case Expr.KeyDown(key) => s"love.keyboard.isDown(\"$key\")"

    case Expr.StateRead(key) =>
      if inCondition then s"e[\"$key\"]"
      else s"-- obsState(\"$key\") used outside animRule condition"

    case Expr.GlobalRead(key) =>
      if inCondition then s"globals[\"$key\"]"
      else s"-- obsGlobal(\"$key\") used outside condition"

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

  // Statement emission
  def emitStatement(stmt: Statement, indent: Int): String =
    val pad = "  " * indent
    stmt match
      case Bind(varName, effect) =>
        s"${pad}local $varName = ${emitYield(effect)}"

      case Perform(effect) =>
        s"${pad}${emitYield(effect)}"

      case Configure(_) =>
        "" // shouldnt be in onUpdate

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

      case Loop(body) => // TODO
        s"""${pad}while true do
           |${emitScript(body, indent + 1)}
           |${pad}end""".stripMargin

      case Return(value) => // TODO
        s"${pad}return ${emitExpr(value)}"

      case HandleWith(handler, body) =>
        val values = handler.handles.map { (k, v) =>
          s"${k.toLowerCase} = ${emitExpr(v)}"
        }.mkString(", ")
        val impls = handler.impls.map { (k, f) =>
          s"${k.toLowerCase}_impl = ${emitImpl(f)}"
        }.mkString(", ")
        val allFields = List(values, impls).filter(_.nonEmpty).mkString(", ")
        val nameComment = handler.name.map(n => s" -- $n").getOrElse("")
        val bodyLua = emitScript(body, indent)
        if bodyLua.nonEmpty then
          s"""${pad}coroutine.yield("PushHandler", {$allFields})$nameComment
             |$bodyLua
             |${pad}coroutine.yield("PopHandler")""".stripMargin
        else
          s"""${pad}coroutine.yield("PushHandler", {$allFields})$nameComment"""

      case Noop => // TODO
        ""

  def emitYield(effect: Effect): String = effect match
    case Effect.Move(dx, dy)                 => s"coroutine.yield(\"Move\", ${emitExpr(dx)}, ${emitExpr(dy)})"
    case Effect.Jump()                       => s"coroutine.yield(\"Jump\")"
    case Effect.Gravity()                    => s"coroutine.yield(\"Gravity\")"
    case Effect.Despawn()                    => s"coroutine.yield(\"Despawn\")"
    case Effect.Draw()                       => s"coroutine.yield(\"Draw\")"
    case Effect.SetState(key, v)             => s"coroutine.yield(\"SetState\", \"$key\", ${emitExpr(v)})"
    case Effect.GetState(key)                => s"coroutine.yield(\"GetState\", \"$key\")"
    case Effect.SetGlobal(key, v)            => s"coroutine.yield(\"SetGlobal\", \"$key\", ${emitExpr(v)})"
    case Effect.GetGlobal(key)               => s"coroutine.yield(\"GetGlobal\", \"$key\")"
    case Effect.SetStateOf(targetId, key, v) => s"coroutine.yield(\"SetStateOf\", \"$targetId\", \"$key\", ${emitExpr(v)})"
    case Effect.GetStateOf(targetId, key)    => s"coroutine.yield(\"GetStateOf\", \"$targetId\", \"$key\")"
    case Effect.Collides(target)             => s"coroutine.yield(\"Collides\", ${emitExprAsString(target)})"
    case Effect.Camera()                     => s"coroutine.yield(\"Camera\")"
    case Effect.Custom(name, _)              => s"coroutine.yield(\"$name\")"
    case Effect.Query(name)                  => s"coroutine.yield(\"$name\")"
    case Effect.ShowState(key)               => s"coroutine.yield(\"ShowState\", \"$key\")"
    case Effect.SetSize(w, h)                => s"coroutine.yield(\"SetSize\", ${emitExpr(w)}, ${emitExpr(h)})"
    case Effect.SpawnAt(tpl, x, y)           => s"coroutine.yield(\"SpawnAt\", \"$tpl\", ${emitExpr(x)}, ${emitExpr(y)})"

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
       |${emitScript(script, indent = 1)}
       |end)""".stripMargin

  // Direct mode emission (for custom effects, handler impls, trigger scripts)
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

      case _ => s"${pad}-- unsupported in direct mode"

  def emitDirectScript(script: Script, indent: Int = 0): String =
    script.statements
      .map(emitDirectStatement(_, indent))
      .filter(_.nonEmpty)
      .mkString("\n")

  // Shared impl emitter for handler 'via' overrides and custom effect bodies
  def emitImpl(f: (Expr, Expr) => Script): String =
    s"""function(task_id, resolved, dt)
       |${emitDirectScript(f(Expr.Var("resolved"), Expr.Var("dt")), indent = 1)}
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

  // Trigger scripts run as inline lambdas inside the region table
  def emitTriggerScript(script: Script): String =
    if script.statements.isEmpty then ""
    else emitDirectScript(script).replace("\n", "; ")