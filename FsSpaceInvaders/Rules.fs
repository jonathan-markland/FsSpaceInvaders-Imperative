module Rules

open Geometry
open GamePlayStateTypes

// Hint:  Things common to drawing and hit-testing.

let InitialHiScore      = 1000
let InvaderRowsCount    = 3
let InvadersPerRow      = 10
let InitialInvaderCount = InvadersPerRow * InvaderRowsCount

/// The invader's colour can be calculated from its DogTag.
let InvaderColour invader =
    match invader with
        | { DogTag = DogTag(tag) } ->
            if (tag &&& 1) = 1 then RedInvader else BlueInvader

