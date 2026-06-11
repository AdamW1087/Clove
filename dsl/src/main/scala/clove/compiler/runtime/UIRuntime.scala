package clove.compiler.runtime

import clove.ast.*
import clove.compiler.WorldFeatures

object UIRuntime:

  // uiDrawList declaration
  def drawListDecl(f: WorldFeatures): String =
    if f.usesUI then "local uiDrawList = {}" else ""

  // uiDrawList reset
  def drawListClear(f: WorldFeatures): String =
    if f.usesUI then "  uiDrawList = {}" else ""

  // Dispatch table entries
  def dispatchEntries(f: WorldFeatures): List[(Boolean, String)] =
    List(
      f.usesUIBar     -> "bar",
      f.usesUILabel   -> "label",
      f.usesUISprites -> "sprites",
      f.usesUISlots   -> "slots",
      f.usesUIImage   -> "image",
    ).map { (enabled, name) =>
      enabled -> s"""|  ui$name = function(task, er, a, b, c, dt)
                     |    a.type = "$name"
                     |    table.insert(uiDrawList, a)
                     |  end,""".stripMargin
    }

  // Render block
  def renderBlock(f: WorldFeatures): String =
    if !f.usesUI then return ""

    val barCase = if f.usesUIBar then
      """|    if item.type == "bar" then
         |      love.graphics.setColor(item.rb, item.gb, item.bb, 1)
         |      love.graphics.rectangle("fill", item.x, item.y, item.w, item.h)
         |      local ratio = math.max(0, math.min(1, item.value / item.max))
         |      love.graphics.setColor(item.r, item.g, item.b, 1)
         |      love.graphics.rectangle("fill", item.x, item.y, item.w * ratio, item.h)
         |      love.graphics.setColor(1, 1, 1, 1)""".stripMargin
    else "    if false then"

    val labelCase = if f.usesUILabel then
      """|    elseif item.type == "label" then
         |      love.graphics.setColor(item.r, item.g, item.b, 1)
         |      local text = item.prefix
         |      if item.value ~= nil then text = text .. tostring(item.value) end
         |      love.graphics.print(text, item.x, item.y)
         |      love.graphics.setColor(1, 1, 1, 1)""".stripMargin
    else ""

    val spritesCase = if f.usesUISprites then
      """|    elseif item.type == "sprites" then
         |      local img = images[item.image]
         |      if img then
         |        local count = math.max(0, math.floor(item.count))
         |        for s = 0, count - 1 do
         |          love.graphics.setColor(1, 1, 1, 1)
         |          love.graphics.draw(img,
         |            item.x + s * (item.w + item.spacing), item.y,
         |            0, item.w / img:getWidth(), item.h / img:getHeight())
         |        end
         |      end""".stripMargin
    else ""

    val slotsCase = if f.usesUISlots then
      """|    elseif item.type == "slots" then
         |      for s, imgPath in ipairs(item.images) do
         |        local slotX = item.x + (s - 1) * (item.size + item.spacing)
         |        if s == math.floor(item.selected) then
         |          love.graphics.setColor(0.9, 0.8, 0.2, 1)
         |        else
         |          love.graphics.setColor(0.3, 0.3, 0.3, 1)
         |        end
         |        love.graphics.rectangle("fill", slotX, item.y, item.size, item.size)
         |        local img = images[imgPath]
         |        if img then
         |          love.graphics.setColor(1, 1, 1, 1)
         |          love.graphics.draw(img, slotX, item.y, 0,
         |            item.size / img:getWidth(), item.size / img:getHeight())
         |        end
         |      end
         |      love.graphics.setColor(1, 1, 1, 1)""".stripMargin
    else ""

    val imageCase = if f.usesUIImage then
      """|    elseif item.type == "image" then
         |      local img = images[item.image]
         |      if img then
         |        love.graphics.setColor(item.r, item.g, item.b, 1)
         |        love.graphics.draw(img, item.x, item.y, 0,
         |          item.w / img:getWidth(), item.h / img:getHeight())
         |        love.graphics.setColor(1, 1, 1, 1)
         |      end""".stripMargin
    else ""

    s"""|  love.graphics.setColor(1, 1, 1, 1)
        |  for i, item in ipairs(uiDrawList) do
        |$barCase
        |$labelCase
        |$spritesCase
        |$slotsCase
        |$imageCase
        |    end
        |  end""".stripMargin