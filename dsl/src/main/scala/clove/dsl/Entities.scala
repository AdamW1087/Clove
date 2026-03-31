package clove.dsl

import clove.ast.*

def entity(name: String): Expr = Expr.Var(name)