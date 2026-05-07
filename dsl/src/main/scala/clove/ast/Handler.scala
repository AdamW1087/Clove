package clove.ast

case class Handler(
  name: Option[String] = None,
  handles: Map[String, Expr] = Map.empty,
  impls: Map[String, (Expr, Expr) => Script] = Map.empty
)