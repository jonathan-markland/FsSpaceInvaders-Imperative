module ScreenLayout

open Geometry
open Dimensions

// Horizontally

let HeadingScoreX   = 0<wu>
let HeadingHiScoreX = ScreenWidth / 3
let HeadingLevelX   = (ScreenWidth * 2) / 3
let HeadingLivesX   = ScreenWidth

let MothershipCentreStartX   = -MothershipWidth
let MothershipCentreEndX     = ScreenWidth + MothershipWidth

let ShipCentreLeftmostX      = ShipWidth
let ShipCentreRightmostX     = ScreenWidth - ShipCentreLeftmostX

// Vertically

let ScoreboardTitlesTopY      = CharBlock / 2
let ScoreboardValuesTopY      = ScoreboardTitlesTopY + 9<wu>

let MotherShipTopY      = 3 * CharBlock
let InvadersTopY        = 5 * CharBlock

let ShipTopY            = ScreenHeight - (CharBlock + ShipHeight)
let BulletStartY        = ShipTopY - BulletHeight
let BulletEndY          = 3 * CharBlock

