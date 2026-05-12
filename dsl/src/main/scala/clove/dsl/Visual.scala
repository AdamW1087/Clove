package clove.dsl

// Describes how an image is drawn over a region's rectangular area
enum VisualMode:
  case Stretch // Scale image to fill region w/h exactly
  case Tile    // Repeat image to fill region w/h (at tileSize, defaulting to natural image size)
  case Sprite  // Draw once at region x/y, top-left aligned, natural size

case class Visual(
  path: String,
  mode: VisualMode,
  tileSize: Option[(Double, Double)] = None // None = use image's natural dimensions
)