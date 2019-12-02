module Rules

open Geometry
open GamePlayStateTypes

// Hint:  Things common to drawing and hit-testing.

let InitialHiScore      = 1000
let InvaderRowsCount    = 3
let InvadersPerRow      = 8
let InitialInvaderCount = InvadersPerRow * InvaderRowsCount

/// The invader's colour can be calculated from its DogTag.
let InvaderColourFromDogTag (DogTag(tag)) =
    if (tag &&& 1) = 1 then RedInvader else BlueInvader

