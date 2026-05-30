package clove.ast

// An impl override carries a Script body plus the named parameters it expects
//   value effect (via):  payload = Nil          -> body uses resolved, dt
//   state read (onGet):  payload = List("key")  -> local key = ...
//   state write (onSet): payload = List("key", "value")
case class Impl(body: Script, payload: List[String] = Nil)

// Handlers handle effect name -> resolved value, and effect name -> impl override
case class Handler(
  name: Option[String] = None,
  handles: Map[String, Expr] = Map.empty,
  impls: Map[String, Impl] = Map.empty
)