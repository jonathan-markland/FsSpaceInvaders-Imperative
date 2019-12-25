module Rates

open Mechanics

let TimeForMothershipCheck   = TickSpan(5000u)
let TimeForMothershipUpdate  = TickSpan(10u)
let TimeForNewBombCheck      = TickSpan(500u)  // TODO: Should this be run-time computed.  Change this each level?
let TimeForBombUpdateCheck   = TickSpan(5u)
let TimeForReloadShipWeapon  = TickSpan(50u)
let TimeForWholeExplosion    = TickSpan(200u)
let TimeForEndState          = TickSpan(2500u)
