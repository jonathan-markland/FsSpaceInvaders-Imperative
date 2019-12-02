module GamePlay

open Algorithm
open InputEventData
open GamePlayStateTypes
open Rules
open Geometry
open Mechanics
open ScreenLayout
open Dimensions
open Scoring
open Rates


/// Create a fresh game world for a new game.
let NewGameWorld hiScore (timeNow:TickCount) : GameWorld =

    let invaderHorizSpacing = ScreenWidth / (InvadersPerRow + 1)    // NB: Intentionally is integer division, so will truncate.
    let invaderHorizSpan    = invaderHorizSpacing * (InvadersPerRow - 1)   // NB: We centre THIS span, so it looks nice even if division truncated.
    let invaderLeftSide     = (ScreenWidth - invaderHorizSpan) / 2
    
    let NewInvader x y =

        let InitialPositionCentreForInvader x y =
            { 
                xw = (x-1) * invaderHorizSpacing + invaderLeftSide
                yw = InvadersTopY + (y-1) * (InvaderHeight + InvaderVSpacing) 
            }

        {
            DogTag = DogTag(y * InvadersPerRow + x)
            InvaderExtents = (InitialPositionCentreForInvader x y) |> ToCenteredRectangle InvaderWidth InvaderHeight
        }

    {
        GameStartTime = timeNow

        PlayStats =
            {
                HiScore = hiScore
                Level   = 1
                Score   = 0
                Lives   = 3
            }

        Motherships = 
            []

        Invaders = 
            [for y in 1..InvaderRowsCount do
                for x in 1..InvadersPerRow do
                    NewInvader x y]

        Bullets = 
            []

        Ship =
            {
                WeaponReloadStartTimeOpt = None

                ShipExtents =
                    let x = ScreenCentreX - (ShipWidth / 2)
                    let y = ShipTopY
                    { 
                        LeftW   = x
                        TopW    = y
                        RightW  = x + ShipWidth
                        BottomW = y + ShipHeight
                    } 
            }
    }



type FrameResult = GameContinuing | PlayerWon | PlayerLost



let CalculateNextFrameState (world:GameWorld) (input:InputEventData) (timeNow:TickCount) =

    let IncreaseScoreBy n =

        world.PlayStats.Score <- world.PlayStats.Score + n
        if world.PlayStats.Score > world.PlayStats.HiScore then
            world.PlayStats.HiScore <- world.PlayStats.Score

    let NewBulletFiredFromCentrallyAbove someRectangle =

        let leftSide = (HorizontalCentreOf someRectangle) - (BulletWidth / 2)
        let baseY    = someRectangle.TopW

        {
            BulletExtents =
                {
                    LeftW    = leftSide
                    RightW   = leftSide + BulletWidth
                    TopW     = baseY - BulletHeight
                    BottomW  = baseY
                }
        }

    let MoveShip () =

        if input.LeftHeld && (HorizontalCentreOf world.Ship.ShipExtents) > ShipCentreLeftmostX then
            world.Ship.ShipExtents <- world.Ship.ShipExtents |> ShuntedBy -1<wu> 0<wu>

        if input.RightHeld && (HorizontalCentreOf world.Ship.ShipExtents) < ShipCentreRightmostX then
            world.Ship.ShipExtents <- world.Ship.ShipExtents |> ShuntedBy 1<wu> 0<wu>

    let ConsiderBulletFiring () =

        let ConsiderReloadPenalty () =

            match world.Ship.WeaponReloadStartTimeOpt,timeNow with
                | Some(TickCount(startTime)),TickCount(timeNow) -> 
                    if (timeNow - startTime) >= TimeForReloadShipWeapon then
                        world.Ship.WeaponReloadStartTimeOpt <- None
                | None,_ -> ()

        let CheckFireButton () =

            if input.FireJustPressed && world.Ship.WeaponReloadStartTimeOpt |> Option.isNone then
                let updatedBulletList = (NewBulletFiredFromCentrallyAbove world.Ship.ShipExtents) :: world.Bullets
                world.Bullets <- updatedBulletList
                world.Ship.WeaponReloadStartTimeOpt <- Some(timeNow)

        ConsiderReloadPenalty ()
        CheckFireButton ()

    let UpdateBullets () =

        let ApplyUpwardMovementToBullet b =
            b.BulletExtents <- b.BulletExtents |> ShuntedBy 0<wu> -1<wu>

        let WhereBulletStillBelowTopmostPosition bullet =
            bullet.BulletExtents.TopW > BulletEndY

        let bulletsStillLive = world.Bullets |> List.filter WhereBulletStillBelowTopmostPosition   // TODO: optimise for case where all are on screen still

        bulletsStillLive |> List.iter ApplyUpwardMovementToBullet

        world.Bullets <- bulletsStillLive

    let ConsiderShotInvaders () =

        let deadBullets,deadInvaders = CollisionsBetween world.Bullets world.Invaders AreaOfBullet AreaOfInvader

        let scoreIncrease = (List.length deadInvaders) * ScoreForKillingInvader
        // TODO: explosions.

        let survingInvaders = world.Invaders |> List.filter (NotInList deadInvaders DogTagOfInvader)  // TODO: Prepare to return same list favouring no removals

        let survingBullets = world.Bullets |> List.filter (NotInList deadBullets AreaOfBullet)  // TODO: Prepare to return same list favouring no removals

        world.Bullets <- survingBullets
        world.Invaders <- survingInvaders
        IncreaseScoreBy scoreIncrease

    let ConsiderShotMothership () =

        // TODO: Performance optimise:  Don't do any of this if no motherships (a common case)

        let deadBullets,deadMotherships = CollisionsBetween world.Bullets world.Motherships AreaOfBullet AreaOfMothership

        let scoreIncrease = (List.length deadMotherships) * ScoreForKillingMothership
        // TODO: explosions.

        let survingMotherships = world.Motherships |> List.filter (NotInList deadMotherships AreaOfMothership)  // TODO: Prepare to return same list favouring no removals

        let survingBullets = world.Bullets |> List.filter (NotInList deadBullets AreaOfBullet)  // TODO: Prepare to return same list favouring no removals

        world.Bullets <- survingBullets
        world.Motherships <- survingMotherships
        IncreaseScoreBy scoreIncrease

    let MoveInvaders () =
    
        let (TickCount(ticks)) = timeNow

        let dx = if (ticks &&& 16u) = 0u then 1<wu> else -1<wu>
        let dy = if (ticks &&& 31u) = 0u then 1<wu> else 0<wu>

        world.Invaders |> List.iter (fun invader ->
            let old = invader.InvaderExtents
            invader.InvaderExtents <-
                {
                    LeftW     = old.LeftW   + dx
                    TopW      = old.TopW    + dy
                    RightW    = old.RightW  + dx
                    BottomW   = old.BottomW + dy
                }
            )

    let MoveMotherships () =

        let dx = 1<wu>

        world.Motherships |> List.iter (fun mothership ->
            let old = mothership.MothershipExtents
            mothership.MothershipExtents <- { old with LeftW = old.LeftW + dx ; RightW = old.RightW + dx }
            )

        let atFinishPosition mothership =
            mothership.MothershipExtents.RightW = (MothershipCentreEndX + MothershipWidth / 2)

        if world.Motherships |> List.exists atFinishPosition then
            let survivingMotherships =
                world.Motherships |> List.filter (fun mothership -> not (mothership |> atFinishPosition))
            world.Motherships <- survivingMotherships

    let NoInvadersLeft () =
    
        world.Invaders.IsEmpty
    
    let InvaderAtLowestLevel () =

        let atLowestLevel invader = invader.InvaderExtents.BottomW >= ShipTopY
        world.Invaders |> List.exists (fun invader -> invader |> atLowestLevel)


    MoveShip ()
    ConsiderBulletFiring ()
    UpdateBullets ()
    ConsiderShotInvaders ()
    ConsiderShotMothership ()
    MoveInvaders ()
    MoveMotherships ()
    // TODO:  Drop bombs
    // TODO:  Consider bombed ship or collided ship
    // TODO:  Move bombs / terminate

    if NoInvadersLeft () then 
        PlayerWon
    else if InvaderAtLowestLevel () then
        PlayerLost
    else
        GameContinuing


/// TODO: Do we pass through world-units?  And let the renderer decide mapping?  Is this wrong to cast to int?
[<Struct>]
type RenderActions =
    | DrawInvader of l1:int * t1:int * dogTag:DogTag
    | DrawShip of l2:int * t2:int
    | DrawBullet of l3:int * t3:int
    | DrawMothership of l4:int * t4:int
    | ClearScreen
    // TODO: | DrawText of l5:int * t5:int * message:string


let BulletPositionOnTopOfShip theShip =

    let shipL = theShip.ShipExtents.LeftW
    let shipT = theShip.ShipExtents.TopW

    let bleft = shipL + ((ShipWidth - BulletWidth)/2)
    let btop  = shipT - BulletHeight

    (bleft,btop)
    

let InWorldUnits (a,b) =  // TODO: Don't want to have to do this -- pass world units through the drawing interface!
    int a , int b


let RenderGamePlay renderer (gameWorld:GameWorld) =

    renderer (ClearScreen)

    gameWorld.Motherships |> List.iter 
        (fun motherShip -> 
            renderer (
                DrawMothership(
                    int motherShip.MothershipExtents.LeftW,
                    int motherShip.MothershipExtents.TopW)))

    gameWorld.Invaders |> List.iter
        (fun invader -> 
            renderer (
                DrawInvader(
                    int invader.InvaderExtents.LeftW,
                    int invader.InvaderExtents.TopW,
                    invader.DogTag)))

    let theShip = gameWorld.Ship
    let shipL = int theShip.ShipExtents.LeftW
    let shipT = theShip.ShipExtents.TopW

    renderer (DrawShip(shipL, int shipT))

    match theShip.WeaponReloadStartTimeOpt with
        | Some(_) -> ()
        | None    -> renderer (DrawBullet ((BulletPositionOnTopOfShip theShip) |> InWorldUnits))

    gameWorld.Bullets |> List.iter
        (fun bullet -> 
            renderer (
                DrawBullet(
                    int bullet.BulletExtents.LeftW,
                    int bullet.BulletExtents.TopW)))

    // TODO: RenderTextString 


            