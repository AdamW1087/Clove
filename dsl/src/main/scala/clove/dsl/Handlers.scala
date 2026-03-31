package clove.dsl

import clove.ast.*

def gravityHandler(strength: Double): Handler =
  Handler("gravityHandler", Map("Gravity" -> Expr.Num(strength)))

def speedHandler(speed: Double): Handler =
  Handler("speedHandler", Map("Speed" -> Expr.Num(speed)))
