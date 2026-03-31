package clove.compiler

import clove.ast.*

object LuaEmitter:

  def emitExpr(expr: Expr): String = expr match
    case Expr.Num(v)            => v.toString
    case Expr.Str(v)            => s"\"$v\""
    case Expr.Bool(v)           => v.toString
    case Expr.Var(name)         => name
    case Expr.BinOp(op, l, r)  => s"(${emitExpr(l)} $op ${emitExpr(r)})"
    case Expr.Not(e)            => s"not (${emitExpr(e)})"
    case Expr.KeyDown(key) => s"love.keyboard.isDown(\"$key\")"
    case Expr.Collides(a, b) => 
      s"checkCollision(entities[${emitExprAsString(a)}], entities[${emitExprAsString(b)}])"

  def emitScript(script: Script, indent: Int = 0): String =
    val pad = "  " * indent
    script match
      case Script.Noop =>
        ""

      case Script.Return(value) =>
        s"${pad}return ${emitExpr(value)}"

      case Script.Perform(effect) =>
        emitEffect(effect, indent)

      case Script.Seq(first, second) =>
        val f = emitScript(first, indent)
        val s = emitScript(second, indent)
        if f.isEmpty then s
        else if s.isEmpty then f
        else s"$f\n$s"

      case Script.When(cond, body) =>
        s"""${pad}if ${emitExpr(cond)} then
           |${emitScript(body, indent + 1)}
           |${pad}end""".stripMargin

      case Script.Loop(body) =>
        s"""${pad}while true do
           |${emitScript(body, indent + 1)}
           |${pad}end""".stripMargin

      case Script.WithHandler(handler, body) =>
        emitScoped(handler, body, indent)

  def emitEffect(effect: Effect, indent: Int): String =
    val pad = "  " * indent
    effect match
      case Effect.Spawn(entity) =>
        s"${pad}handler.spawn(${emitExprAsString(entity)})"
      
      case Effect.Despawn(entity) =>
        s"${pad}handler.despawn(${emitExprAsString(entity)})"
      
      case Effect.Move(entity, dx, dy) =>
        s"${pad}handler.move(${emitExprAsString(entity)}, ${emitExpr(dx)}, ${emitExpr(dy)})"
      
      case Effect.Draw(entity) =>
        s"${pad}handler.draw(${emitExprAsString(entity)})"
      
      case Effect.SetState(entity, key, value) =>
        s"${pad}handler.setState(${emitExprAsString(entity)}, \"$key\", ${emitExpr(value)})"
      
      case Effect.GetState(entity, key, result) =>
        s"${pad}local $result = handler.getState(${emitExprAsString(entity)}, \"$key\")"

  def emitExprAsString(expr: Expr): String = expr match
    case Expr.Var(name) => s"\"$name\""
    case other          => emitExpr(other)


  def emitScoped(handler: Handler, body: Script, indent: Int): String =
    val pad = "  " * indent
    val overrides = handler.handles.map { (k, v) =>
      s"${pad}  $k = ${emitExpr(v)}"
    }.mkString(",\n")

    s"""${pad}local ${handler.name} = {
       |$overrides
       |${pad}}
       |${pad}do
       |${emitScript(body, indent + 1)}
       |${pad}end""".stripMargin
