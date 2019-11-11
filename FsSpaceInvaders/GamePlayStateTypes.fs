module GamePlayStateTypes

open Mechanics


type GamePlayStats =
    {
        HiScore: int
        Level:   int
        Score:   int
        Lives:   int
    }





type Ship = 
    {
        ShipX: float

        /// If None, the ship's weapon is ready.
        /// If Some() it gives the tick count at which
        /// the reload penalty began.
        WeaponReloadStartTimeOpt: TickCount option
    }


type InvaderColour = RedInvader | BlueInvader
type DogTag    = DogTag of int


/// Record to represent invader instance, created
/// only when the invader instance appears at the
/// start of the game level.
type Invader =
    {
        /// Used to identify instance, calculate
        /// its relative position in the formation,
        /// and calculate a colour.
        DogTag:  DogTag
    }


/// Values that apply to all motherships.
/// Can be calculated at game start, then never changes.
type MothershipConstants =
    {
        /// Number of ticks to get from start to end point.
        /// End point is the right of the screen.
        /// Allows horiozontal position calculation.
        MothershipDuration: TickCount
    }


/// Record to represent mothership instance, created
/// only when the mothership instance appears.
type Mothership =
    {
        /// Used to calculate the mothership's position, 
        /// given the start time tick at which it first
        /// appeared.
        MothershipAppearanceTime: TickCount
    }


/// Values that apply to all bullets.
/// Can be calculated at game start, then never changes.
type BulletConstants =
    {
        /// Number of ticks to get from start to end point.
        /// End point is the top of the screen at BulletFinishY.
        /// Allows vertical position calculation.
        BulletDuration: TickCount
    }


/// Record to represent bullet instance, created
/// only when the bullet instance appears.
type Bullet =
    {
        /// Used to calculate the bullet's position as
        /// it travels upwards from the top of the ship, 
        /// given the start time tick at which it first
        /// appeared.
        BulletAppearanceTime: TickCount

        /// Horizontal position of bullet on the screen,
        /// in Unit Square Space.  Never changes once fired.
        BulletX : float
    }


type GameWorld =
    {
        GameStartTime:           TickCount
        PlayStats:               GamePlayStats
        MothershipStateOpt:      Mothership option
        InvadersList:            Invader list
        Bullets:                 Bullet list
        ShipState:               Ship
    }


type Screen =
    | WelcomeScreen
    | GamePlayScreen of GameWorld
    | GameOverScreen





