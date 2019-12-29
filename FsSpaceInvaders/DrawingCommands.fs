module DrawingCommands

open Geometry
open GamePlayTypes
open Fonts

/// Drawing commands are communicated from the engine to the concrete renderer.
/// These use World Coordinates for positions and sizes, and the renderer is at 
/// liberty to interpret these requests in any way is sees fit, when projecting
/// the image onto the screen.
[<Struct>]
type DrawingCommand =
    | DrawInvader    of l1:int<wu> * t1:int<wu> * dogTag:DogTag
    | DrawShip       of l2:int<wu> * t2:int<wu>
    | DrawBullet     of l3:int<wu> * t3:int<wu>
    | DrawMothership of l4:int<wu> * t4:int<wu>
    | DrawBomb       of l6:int<wu> * t6:int<wu>
    | DrawExplosion  of explosion:Explosion
    | DrawText       of x:int<wu> * topY5:int<wu> * message:string * textHAlign:TextHAlignment * textVAlign:TextVAlignment
    | TitleBackground
    | GameplayBackground
    | GameOverBackground
    | NextLevelBackground
    | LifeOverBackground
