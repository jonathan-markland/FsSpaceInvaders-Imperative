module GamePlayStateTypes

open Geometry
open Mechanics


type GamePlayStats =
    {
        mutable HiScore: int
        mutable Level:   int
        mutable Score:   int
        mutable Lives:   int
    }


type Ship = 
    {
        mutable ShipExtents: RectangleW

        /// If None, the ship's weapon is ready.
        /// If Some() it gives the tick count at which
        /// the reload penalty began.
        mutable WeaponReloadStartTimeOpt: TickCount option
    }


type InvaderColour = RedInvader | BlueInvader

type DogTag = DogTag of int


/// Record to represent invader instance, created
/// only when the invader instance appears at the
/// start of the game level.
type Invader =
    {
        /// Used to identify instance, calculate
        /// its relative position in the formation,
        /// and calculate a colour.
        DogTag:  DogTag

        mutable InvaderExtents: RectangleW
    }

let AreaOfInvader i = i.InvaderExtents
let DogTagOfInvader i = i.DogTag


/// Values that apply to all motherships.
/// Can be calculated at game start, then never changes.
type MothershipConstants =
    {
        /// Number of ticks to get from start to end point.
        /// End point is the right of the screen.
        MothershipDuration: TickCount
    }


/// Record to represent mothership instance, created
/// only when the mothership instance appears.
type Mothership =
    {
        mutable MothershipExtents: RectangleW
    }

let AreaOfMothership m = m.MothershipExtents

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
        mutable BulletExtents: RectangleW
    }

let AreaOfBullet b = b.BulletExtents


/// The Game world state during gameplay.
type GameWorld =
    {
        GameStartTime:              TickCount
        PlayStats:                  GamePlayStats
        mutable Motherships:        Mothership list  // anticipated max one for now, but type-similarity to Invaders allows some uniform handling.
        mutable Invaders:           Invader list
        mutable Bullets:            Bullet list
        Ship:                       Ship
    }


type CrossGameState =
    {
        HiScore: int
    }


type Screen =
    | WelcomeScreen  of CrossGameState
    | GamePlayScreen of CrossGameState * GameWorld
    | GameOverScreen of CrossGameState





