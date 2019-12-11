module Rates

open Mechanics

let TimeForMothershipCheck = TickSpan(5000u)
let TimeForMothershipUpdate = TickSpan(10u)
let TimeForNewBombCheck = TickSpan(255u)
let TimeForBombUpdateCheck = TickSpan(5u)
let TimeForReloadShipWeapon = 50u
