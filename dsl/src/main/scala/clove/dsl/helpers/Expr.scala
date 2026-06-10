package clove.dsl.helpers


import clove.ast.*


// Input (pure Expr constructors)
def keyDown(key: String): Expr =
  Expr.KeyDown(key)

def justPressed(key: String): Expr =
  Expr.JustPressed(key)

val deltaTime: Expr = Expr.DeltaTime


// Observation

def obsState(key: String): Expr = Expr.StateRead(key)
def obsGlobal(key: String): Expr = Expr.GlobalRead(key)
def obsStateOf(id: String, key: String): Expr = Expr.EntityRead(id, key)
def exists(id: String): Expr = Expr.EntityExists(id)


// math expressions

def max(exprs: Expr*): Expr  = Expr.Max(exprs*)
def min(exprs: Expr*): Expr  = Expr.Min(exprs*)
def ceil(expr: Expr): Expr   = Expr.Ceil(expr)
def floor(expr: Expr): Expr  = Expr.Floor(expr)
