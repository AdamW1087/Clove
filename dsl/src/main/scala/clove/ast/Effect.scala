package clove.ast

// Effects that have a key representation
sealed trait EffectKey:
  def name: String

// Effect keys used in handler(Gravity -> 10.0, Jump -> 5.0, ...)
object Key:
  case object Gravity   extends EffectKey { val name = "gravity"   }
  case object Jump      extends EffectKey { val name = "jump"      }
  case object Move      extends EffectKey { val name = "move"      }
  case object Collides  extends EffectKey { val name = "collides"  }
  case object Camera    extends EffectKey { val name = "camera"    }
  case object ShowState extends EffectKey { val name = "showstate" }
  case object SetSize   extends EffectKey { val name = "setsize"   }
  case object Despawn   extends EffectKey { val name = "despawn"   }

// A user-defined custom effect
case class CustomEffect(effectName: String, impl: (Expr, Expr) => Script) extends EffectKey:
  val name = effectName.toLowerCase
  def toEffect: Effect = Effect.Custom(effectName, impl)

// A query effect key
// val isUnderwater = QueryKey("isUnderwater")
// handler(isUnderwater -> false)  as a key
// isUnderwater()                  in script, binds and returns result
case class QueryKey(effectName: String) extends EffectKey:
  val name = effectName.toLowerCase
  def apply()(using b: clove.dsl.ScriptBuilder): Expr =
    val varName = b.nextVar()
    b += Bind(varName, Effect.Query(effectName))
    Expr.Var(varName)

// Contains all Effects/functions to access and manage state
enum Effect:
  // Entity affecting effects
  case Move(dx: Expr, dy: Expr)
  case Jump()
  case Gravity()
  case Despawn()
  case Collides(target: Expr)

  // User defined (via customEffect / QueryKey)
  case Custom(name: String, impl: (Expr, Expr) => Script = (_, _) => Script(List.empty))
  case Query(name: String)

  // State management
  case SetState(key: String, value: Expr)
  case GetState(key: String)
  case SetGlobal(key: String, value: Expr)
  case GetGlobal(key: String)
  case SetSize(width: Expr, height: Expr)

  // Note these contain a string due to if 2 entities needed to access eachother (alternative would be defining both and implementing later)
  case SetStateOf(targetId: String, key: String, value: Expr)
  case GetStateOf(targetId: String, key: String)

  // Others
  case Draw()
  case ShowState(key: String)
  case Camera()
  case SpawnAt(templateName: String, x: Expr, y: Expr)