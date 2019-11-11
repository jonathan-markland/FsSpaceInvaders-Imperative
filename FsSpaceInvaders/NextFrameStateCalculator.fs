module NextFrameStateCalculator

open InputEventData
open GamePlayStateTypes
open Rules
open Mechanics

let NewGameWorld hiScore (timeNow:TickCount) =
    {
        GameStartTime = timeNow

        PlayStats =
            {
                HiScore = hiScore
                Level   = 1
                Score   = 0
                Lives   = 3
            }

        MothershipStateOpt = 
            None

        InvadersList = 
            [1..InitialInvaderCount] |> List.map (fun n -> { DogTag = DogTag(n) })

        Bullets = 
            []

        ShipState =
            {
                ShipX = 0.5
                WeaponReloadStartTimeOpt = None
            }
    }


let CalculateNextFrameState (world:GameWorld) (input:InputEventData) (timeNow:TickCount) =

    // Calculate new ship position

    // Is FIRE pressed?  If so, and WeaponReloadStartTimeOpt is None
    // we can add a bullet.

    // Calculate new positions for bullets, including removals.

    // Do any bullets (in new positions) hit invaders?
    // If so, remove bullet and invader, increase score.

    // Observation:  O(n2) in bullet x invader collision tests.
    // Will need to cache positions of invaders and bullets anyway for convenience!
    
    // If no mothership then:
    //   If time since game start allows for spawing new mother ship
    //   then return Some(new mothership)
    // Else:
    //   Do bullets intersect mothership?
    //     If so, return None for new mother ship status, and increase score.
    //     Else just return new mothership position.

