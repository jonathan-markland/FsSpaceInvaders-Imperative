module Rules

open GamePlayTypes

let InitialHiScore      = 1000
let InvaderRowsCount    = 3
let InvadersPerRow      = 8
let InitialInvaderCount = InvadersPerRow * InvaderRowsCount

let InvaderColourFromDogTag (DogTag(tag)) =
    if (tag &&& 1) = 1 then RedInvader else BlueInvader

