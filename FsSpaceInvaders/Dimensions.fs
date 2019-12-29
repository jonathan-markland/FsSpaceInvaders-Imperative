module Dimensions

open Geometry

let ScreenWidth          = 256<wu>
let ScreenHeight         = 256<wu>

let CharBlock            = 8<wu>

let ScoreboardCharWidth  = 16<wu>   // TODO: use these?
let ScoreboardCharHeight = 8<wu>    // TODO: use these?

let MothershipWidth      = 32<wu>
let MothershipHeight     = 8<wu>
let MothershipStep       = 2<wu>

let InvaderWidth         = 16<wu>
let InvaderHeight        = 8<wu>
let InvaderVSpacing      = 4<wu>
let InvaderWiggleStep    = 1<wu>
let InvaderAdvanceStep   = 8<wu>

let BulletWidth          = 4<wu>
let BulletHeight         = 8<wu>
let BulletStep           = 8<wu>

let BombWidth            = 4<wu>
let BombHeight           = 6<wu>
let BombStep             = 4<wu>

let ShipWidth            = 16<wu>
let ShipHeight           = 32<wu>
let ShipMovementStep     = 5<wu>

let ScreenCentreX        = ScreenWidth / 2
