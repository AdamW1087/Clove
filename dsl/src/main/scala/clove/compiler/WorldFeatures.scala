package clove.compiler

// Describes which runtime features are required by a compiled world
case class WorldFeatures(
  usesGravity:     Boolean,
  usesJump:        Boolean,
  usesMove:        Boolean,
  usesCollides:    Boolean,
  usesCamera:      Boolean,
  usesGlobals:     Boolean,
  usesAnimations:  Boolean,
  usesHandlers:    Boolean,
  usesVisuals:     Boolean,
  usesEntityReads: Boolean,
  usesCrossEntityReads:  Boolean,
  usesCrossEntityWrites: Boolean,
  usesSpawnAt:           Boolean,
  usesUI:                Boolean,
  usesUIBar:             Boolean,
  usesUILabel:           Boolean,
  usesUISprites:         Boolean,
  usesUISlots:           Boolean,
  usesUIImage:           Boolean,
  usesJustPressed:       Boolean,
  usesSound:             Boolean,
  usesMusic:             Boolean,
)