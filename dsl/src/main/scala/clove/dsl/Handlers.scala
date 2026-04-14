package clove.dsl

import clove.ast.*


// TODO: Proper default values

def createHandler(name: Option[String], handles: Map[String, Expr]): Handler =
  Handler(name, handles)


def handler(handles: (String, Expr)*): Handler =
  createHandler(None, handles.toMap)

def handler(name: String, handles: (String, Expr)*): Handler =
  createHandler(Some(name), handles.toMap)


def gravityHandler(strength: Double, name: Option[String] = None): Handler =
  createHandler(name, Map("Gravity" -> Expr.Num(strength)))

def moveHandler(speed: Double, name: Option[String] = None): Handler =
  createHandler(name, Map("Move" -> Expr.Num(speed)))