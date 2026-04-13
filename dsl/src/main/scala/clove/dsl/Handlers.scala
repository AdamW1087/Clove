package clove.dsl

import clove.ast.*


def createHandler(name: Option[String], handles: Map[String, Expr]): Handler =
  Handler(name, handles)


def handler(handles: (String, Expr)*): Handler =
  createHandler(None, handles.toMap)

def handler(name: String, handles: (String, Expr)*): Handler =
  createHandler(Some(name), handles.toMap)


def gravityHandler(strength: Double, name: Option[String] = None): Handler =
  createHandler(name, Map("Gravity" -> Expr.Num(strength)))

// TODO: drag
def speedHandler(speed: Double, name: Option[String] = None): Handler =
  createHandler(name, Map("Speed" -> Expr.Num(speed)))
