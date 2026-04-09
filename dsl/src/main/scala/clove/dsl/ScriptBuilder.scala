package clove.dsl

import clove.ast.*
import scala.collection.mutable.ListBuffer

class ScriptBuilder:
  private val steps = ListBuffer[Statement]()
  private var resultCounter = 0

  def +=(stmt: Statement): Unit = steps += stmt

  def nextVar(): String =
    val name = s"res_$resultCounter"
    resultCounter += 1
    name

  def build(): Script = Script(steps.toList)

def script(body: ScriptBuilder ?=> Unit): Script =
  val builder = ScriptBuilder()
  body(using builder)
  builder.build()
