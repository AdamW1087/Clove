package clove.compiler

// Describes which runtime features are required by a compiled world
case class WorldFeatures(
  usesGravity:    Boolean,
  usesJump:       Boolean,
  usesMove:       Boolean,
  usesCollides:   Boolean,
  usesCamera:     Boolean,
  usesShowState:  Boolean,
  usesGlobals:    Boolean,
  usesTriggers:   Boolean,
  usesAnimations: Boolean,
  usesHandlers:   Boolean,
  usesVisuals:    Boolean
)