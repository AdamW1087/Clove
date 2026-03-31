package clove.ast

case class Handler(
  name: String,
  handles: Map[String, Expr]
)
