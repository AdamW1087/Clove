package clove.dsl

import clove.ast.Expr

extension (e: Expr)
  def *(other: Expr): Expr = Expr.BinOp("*", e, other)
  def +(other: Expr): Expr = Expr.BinOp("+", e, other)
  def -(other: Expr): Expr = Expr.BinOp("-", e, other)
  def /(other: Expr): Expr = Expr.BinOp("/", e, other)
extension (d: Double)
  def *(expr: Expr): Expr = Expr.BinOp("*", Expr.Num(d), expr)
  def +(expr: Expr): Expr = Expr.BinOp("+", Expr.Num(d), expr)
  def -(expr: Expr): Expr = Expr.BinOp("-", Expr.Num(d), expr)
  def /(expr: Expr): Expr = Expr.BinOp("/", Expr.Num(d), expr)