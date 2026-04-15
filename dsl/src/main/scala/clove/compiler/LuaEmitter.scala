package clove.compiler

import clove.ast.*

object LuaEmitter:

  def emitExpr(expr: Expr): String = expr match
    case Expr.Num(v)                => v.toString
    case Expr.Str(v)                => s"\"$v\""
    case Expr.Bool(v)               => v.toString
    case Expr.Var(name)             => name
    case Expr.BinOp(op, l, r)       => s"(${emitExpr(l)} $op ${emitExpr(r)})"
    case Expr.Not(e)                => s"not (${emitExpr(e)})"
    case Expr.KeyDown(key)          => s"love.keyboard.isDown(\"$key\")"
    case Expr.Propagate(expr)       => s"{value = ${emitExpr(expr)}, propagate = true}"

  def emitExprAsString(expr: Expr): String = expr match
    case Expr.Var(name) => s"\"$name\""
    case other          => emitExpr(other)

  def emitStatement(stmt: Statement, indent: Int): String =
    val pad = "  " * indent
    stmt match
      case Bind(varName, effect) =>
        s"${pad}local $varName = ${emitYield(effect)}"

      case Perform(effect) =>
        s"${pad}${emitYield(effect)}"

      case If(cond, thenBranch) =>
        s"""${pad}if ${emitExpr(cond)} then
           |${emitScript(thenBranch, indent + 1)}
           |${pad}end""".stripMargin

      case Loop(body) => // TODO
        s"""${pad}while true do
           |${emitScript(body, indent + 1)}
           |${pad}end""".stripMargin

      case Return(value) => // TODO
        s"${pad}return ${emitExpr(value)}"

      case HandleWith(handler, body) =>
        val overrides = handler.handles.map { (k, v) =>
          s"${k.toLowerCase} = ${LuaEmitter.emitExpr(v)}"
        }.mkString(", ")
        val nameComment = handler.name.map(n => s" -- $n").getOrElse("")
        val bodyLua = emitScript(body, indent)
        val bodySep = if bodyLua.nonEmpty then s"\n$bodyLua" else ""
        s"""${pad}coroutine.yield("PushHandler", {$overrides})$nameComment$bodySep"""

      case Noop => // TODO
        ""

  def emitYield(effect: Effect): String = effect match
    case Effect.Move(dx, dy)     => s"coroutine.yield(\"Move\", ${emitExpr(dx)}, ${emitExpr(dy)})"
    case Effect.Jump()           => s"coroutine.yield(\"Jump\")"
    case Effect.Gravity()        => s"coroutine.yield(\"Gravity\")"
    case Effect.Spawn()          => s"coroutine.yield(\"Spawn\")"
    case Effect.Despawn()        => s"coroutine.yield(\"Despawn\")"
    case Effect.Draw()           => s"coroutine.yield(\"Draw\")"
    case Effect.SetState(key, v) => s"coroutine.yield(\"SetState\", \"$key\", ${emitExpr(v)})"
    case Effect.GetState(key)    => s"coroutine.yield(\"GetState\", \"$key\")"
    case Effect.Collides(target) => s"coroutine.yield(\"Collides\", ${emitExprAsString(target)})"
    case Effect.Camera()         => s"coroutine.yield(\"Camera\")"

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

  def emitSpawnScript(entityId: String, script: Script): String =
    script.statements.map {
      case Bind(_, Effect.SetState(key, value)) =>
        s"  entities[\"$entityId\"][\"$key\"] = ${emitExpr(value)}"
      case Perform(Effect.SetState(key, value)) =>
        s"  entities[\"$entityId\"][\"$key\"] = ${emitExpr(value)}"
      case _ => ""
    }.filter(_.nonEmpty).mkString("\n")