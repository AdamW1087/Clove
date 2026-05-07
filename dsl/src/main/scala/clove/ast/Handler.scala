package clove.ast

// Class for handler, having name of effect, the value it returns and any overriding implementation respectively
case class Handler(
  name: Option[String] = None,
  handles: Map[String, Expr] = Map.empty,
  impls: Map[String, (Expr, Expr) => Script] = Map.empty
)