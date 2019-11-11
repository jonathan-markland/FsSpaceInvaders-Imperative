module Rules

open Geometry
open GamePlayStateTypes
open Mechanics

// Hint:  Things common to drawing and hit-testing.

let InitialHiScore      = 1000
let InvaderRowsCount    = 3
let InvadersPerRow      = 10
let InitialInvaderCount = InvadersPerRow * InvaderRowsCount
let MothershipStartX    = 0.1
let MothershipEndX      = 0.9
let MotherShipY         = 0.1
let BulletStartY        = 0.8
let BulletEndY          = 0.01
let ShipY               = 0.9


// --- SPRITE DIMENSIONS ----------------------------------------------------------------

let ShipBoundingBox centre       = centre |> ToCenteredRectangle 0.05 0.1
let MothershipBoundingBox centre = centre |> ToCenteredRectangle 0.1  0.025
let InvaderBoundingBox centre    = centre |> ToCenteredRectangle 0.05 0.025
let BulletBoundingBox centre     = centre |> ToCenteredRectangle 0.01 0.002


// --- CALCULATED ATTRIBUTES ----------------------------------------------------------------

/// The invader's colour can be calculated from its DogTag.
let InvaderColour invader =
    match invader with
        | { DogTag = DogTag(tag) } ->
            if (tag &&& 1) = 1 then RedInvader else BlueInvader


// --- POSITIONS OF SPRITES ----------------------------------------------------------------

/// We can calculate the center point of the mothership
/// given the elapsed time since it first appeared.
/// We only return values for its duration of movement.
let MothershipCentre mothership mothershipConstants timeNow =
    match CalculatePositionLinearly 0.1 0.9 (mothership.MothershipAppearanceTime) (mothershipConstants.MothershipDuration) timeNow with
        | Some(x) -> Some({ cx = x ; cy = MotherShipY })
        | None    -> None


/// We can calculate the centre point of the invader, given
/// its DogTag number.  TODO: have time as input to move by motion delta.
let InvaderCentre invader =
    match invader with
        | { DogTag = DogTag(tag) } ->
            let indexX = double (tag % InvadersPerRow)
            let indexY = double (tag / InvadersPerRow)
            {
                cx = (0.1 * indexX) + 0.05
                cy = (0.0 * indexY) + 0.25
            }


/// Calculates a bullet position, given the time elapsed since it was fired.
let BulletCentre bullet bulletConstants timeNow =
    match CalculatePositionLinearly 
            BulletStartY 
            BulletEndY 
            (bullet.BulletAppearanceTime) 
            (bulletConstants.BulletDuration) 
            timeNow with
        | Some(cy) -> Some({ cx = bullet.BulletX ; cy = cy })
        | None     -> None
      

/// Returns the ship position.
let ShipCentre ship =
    { cx = ship.ShipX ; cy = ShipY }


