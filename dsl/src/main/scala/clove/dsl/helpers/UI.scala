package clove.dsl.helpers

import clove.ast.*
import clove.dsl.ScriptBuilder


// UI widgets (region)

def uiBar(x: Double, y: Double, w: Double, h: Double,
          value: Expr, max: Expr,
          colour:   (Double, Double, Double),
          bgColour: (Double, Double, Double))
         (using b: ScriptBuilder): Unit =
  b += Perform(UI.Bar(x, y, w, h, value, max, colour, bgColour))

def uiLabel(x: Double, y: Double, prefix: String,
            value: Option[Expr] = None,
            colour: (Double, Double, Double))
           (using b: ScriptBuilder): Unit =
  b += Perform(UI.Label(x, y, prefix, value, colour))

def uiSprites(x: Double, y: Double, image: String,
              count: Expr,
              spacing: Double,
              w: Double, h: Double)
             (using b: ScriptBuilder): Unit =
  b += Perform(UI.Sprites(x, y, image, count, spacing, w, h))

def uiSlots(x: Double, y: Double, size: Double,
            images: List[String], selected: Expr,
            spacing: Double)
           (using b: ScriptBuilder): Unit =
  b += Perform(UI.Slots(x, y, size, images, selected, spacing))

def uiImage(x: Double, y: Double, w: Double, h: Double,
            image: String,
            colour: (Double, Double, Double))
           (using b: ScriptBuilder): Unit =
  b += Perform(UI.Image(x, y, w, h, image, colour))
