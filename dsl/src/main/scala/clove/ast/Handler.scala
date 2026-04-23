package clove.ast

 // TODO : extend handler capabilites
case class Handler(
  name: Option[String] = None,
  handles: Map[String, Expr]
)