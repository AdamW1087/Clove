package clove.dsl

import clove.ast.Expr

extension (e: Expr)
  def *(other: Expr): Expr = Expr.BinOp("*", e, other)
  def +(other: Expr): Expr = Expr.BinOp("+", e, other)
  def -(other: Expr): Expr = Expr.BinOp("-", e, other)
  def /(other: Expr): Expr = Expr.BinOp("/", e, other)
  def <(other: Expr): Expr  = Expr.BinOp("<", e, other)
  def >(other: Expr): Expr  = Expr.BinOp(">", e, other)
  def <=(other: Expr): Expr = Expr.BinOp("<=", e, other)
  def >=(other: Expr): Expr = Expr.BinOp(">=", e, other)
  def ===(other: Expr): Expr = Expr.BinOp("==", e, other)
  def &&(other: Expr): Expr = Expr.BinOp("and", e, other)
  def ||(other: Expr): Expr = Expr.BinOp("or", e, other)
  def unary_! : Expr = Expr.Not(e)
  def unary_- : Expr = Expr.Negate(e)
extension (d: Double)
  def *(expr: Expr): Expr = Expr.BinOp("*", Expr.Num(d), expr)
  def +(expr: Expr): Expr = Expr.BinOp("+", Expr.Num(d), expr)
  def -(expr: Expr): Expr = Expr.BinOp("-", Expr.Num(d), expr)
  def /(expr: Expr): Expr = Expr.BinOp("/", Expr.Num(d), expr)