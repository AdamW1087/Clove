package clove.dsl

import clove.ast.*
import scala.collection.mutable.ListBuffer

class ScriptBuilder:
  private val steps = ListBuffer[Script]()

  def +=(script: Script): Unit = 
    steps += script

  def build(): Script =
    steps.toList match
      case Nil        => Script.Noop
      case s :: Nil   => s
      case ss         => ss.reduce(Script.Seq(_, _))

def script(body: ScriptBuilder ?=> Unit): Script =
  val builder = ScriptBuilder()
  body(using builder)
  builder.build()


case class World(setup: Script, regions: List[Region])

class WorldBuilder extends ScriptBuilder:
  val regions = ListBuffer[Region]()
  def addRegion(r: Region): Unit = regions += r

def world(body: WorldBuilder ?=> Unit): World =
  val builder = WorldBuilder()
  body(using builder)
  World(builder.build(), builder.regions.toList)