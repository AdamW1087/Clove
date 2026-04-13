package clove.ast

 // TODO -transformation logic (custom handlers)
case class Handler(
  name: Option[String] = None,
  handles: Map[String, Expr]
)