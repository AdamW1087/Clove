package clove.ast

// Effects that have a key representation
sealed trait EffectKey:
  def name: String

// Effect keys used in handler(Gravity -> 10.0, Jump -> 5.0, ...)
enum Key(val name: String) extends EffectKey:
  case Gravity  extends Key("gravity")
  case Jump     extends Key("jump")
  case Move     extends Key("move")
  case Collides extends Key("collides")
  case Camera   extends Key("camera")
  case SetSize  extends Key("setsize")
  case Despawn  extends Key("despawn")
  case GetState extends Key("getstate")
  case SetState extends Key("setstate")

// A user-defined custom effect
case class CustomEffect[R](effectName: String, impl: (Expr, Expr) => Script) extends EffectKey:
  val name = effectName.toLowerCase
  def toEffect: Effect[R] = Effect.UserEffect[R](name)

// A query effect key
case class QueryKey(effectName: String) extends EffectKey:
  val name = effectName.toLowerCase
  def apply()(using b: clove.dsl.ScriptBuilder): Expr =
    val varName = b.nextVar()
    b += Bind(varName, Effect.UserEffect[Expr](name))
    Expr.Var(varName)

// An Effect a script performs, with resumption type R
sealed trait Effect[+R]

// Continuation, resume an intercepted effect. Only valid inside
// handler impls (onGet/onSet)
sealed trait Continuation[+R] extends Effect[R]

object Effect:
  // Unit responses
  case class Gravity()                                              extends Effect[Unit]
  case class SetState(key: String, value: Expr)                     extends Effect[Unit]
  case class SetGlobal(key: String, value: Expr)                    extends Effect[Unit]
  case class SetSize(width: Expr, height: Expr)                     extends Effect[Unit]
  case class SetStateOf(targetId: String, key: String, value: Expr) extends Effect[Unit]
  case class Camera()                                               extends Effect[Unit]
  case class SpawnAt(templateName: String, x: Expr, y: Expr)        extends Effect[Unit]
  case class PlaySound(path: String)                                extends Effect[Unit]

  // Effects that return an Expr
  case class Move(dx: Expr, dy: Expr)                  extends Effect[Expr]
  case class Jump()                                    extends Effect[Expr]
  case class GetState(key: String)                     extends Effect[Expr]
  case class GetGlobal(key: String)                    extends Effect[Expr]
  case class GetStateOf(targetId: String, key: String) extends Effect[Expr]
  case class Collides(target: Expr)                    extends Effect[Expr]

  // User-defined effect
  // R is Unit for action effects, Expr for query effects
  case class UserEffect[R](name: String) extends Effect[R]

  // Continuation discarding effect
  case class Despawn() extends Effect[Nothing]

  // Resume the default read/write inside a state handler impl
  case class ResumeWrite() extends Continuation[Unit]
  case class ResumeRead()  extends Continuation[Unit]

  // Resume the continuation with a value
  case class ResumeWith(value: Expr) extends Continuation[Unit]

  
// UI effects
sealed trait UI extends Effect[Unit]

object UI:
  // Filled progress bar with background
  case class Bar(
    x: Double, y: Double,
    w: Double, h: Double,
    value: Expr, max: Expr,
    colour:   (Double, Double, Double),
    bgColour: (Double, Double, Double)
  ) extends UI

  // Text label with optional dynamic value appended
  case class Label(
    x: Double, y: Double,
    prefix: String,
    value: Option[Expr] = None,
    colour: (Double, Double, Double) = (1.0, 1.0, 1.0)
  ) extends UI

  // Repeated sprite icons
  case class Sprites(
    x: Double, y: Double,
    image: String,
    count: Expr,
    spacing: Double,
    w: Double,
    h: Double
  ) extends UI

//  TO CHECK

  // Hotbar
  case class Slots(
    x: Double, y: Double,
    size: Double,
    images: List[String],
    selected: Expr,
    spacing: Double
  ) extends UI

  // Raw image draw
  case class Image(
    x: Double, y: Double,
    w: Double, h: Double,
    image: String,
    colour: (Double, Double, Double) = (1.0, 1.0, 1.0)
  ) extends UI