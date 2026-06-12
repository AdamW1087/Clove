package clove.dsl

// Describes how an image is drawn over a regions rectangular area
enum VisualMode:
  case Stretch // Scale to fill region w/h exactly
  case Tile    // Repeat to fill region w/h
  case Sprite  // Draw once at region x/y, aligned at top left, natural size

case class Visual(
  path: String,
  mode: VisualMode,
  tileSize: Option[(Double, Double)] = None
)

given Conversion[(Double, Double, Double), Option[(Double, Double, Double)]] = Some(_)