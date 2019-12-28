module Rates

open Mechanics

let TimeForMothershipCheck   = TickSpan(1000u)
let TimeForNewBombCheck      = TickSpan(40u)  // TODO: Should this be run-time computed.  Change this each level?
let TimeForReloadShipWeapon  = TickSpan(15u)
let TimeForWholeExplosion    = TickSpan(15u)
let TimeForEndState          = TickSpan(250u)

