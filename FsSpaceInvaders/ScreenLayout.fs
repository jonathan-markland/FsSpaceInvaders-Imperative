module ScreenLayout

open Dimensions

// Horizontally

let MothershipCentreStartX   = -MothershipWidth
let MothershipCentreEndX     = ScreenWidth + MothershipWidth

let ShipCentreLeftmostX      = ShipWidth
let ShipCentreRightmostX     = ScreenWidth - ShipCentreLeftmostX

// Vertically

let ScoreboardTopY      = 1 * CharBlock
let MotherShipTopY      = 3 * CharBlock
let InvadersTopY        = 5 * CharBlock

let ShipTopY            = ScreenHeight - (CharBlock + ShipHeight)
let BulletStartY        = ShipTopY - BulletHeight
let BulletEndY          = 3 * CharBlock

