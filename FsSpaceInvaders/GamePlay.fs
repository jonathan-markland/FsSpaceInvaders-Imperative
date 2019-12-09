module GamePlay

open Algorithm
open InputEventData
open GamePlayTypes
open Rules
open Geometry
open Mechanics
open ScreenLayout
open Dimensions
open Scoring
open Rates
open Fonts


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
            InvaderExtents = (InitialPositionCentreForInvader x y) |> RectangleCenteredAboutPoint InvaderWidth InvaderHeight
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
            world.Ship.ShipExtents <- world.Ship.ShipExtents |> RectangleShuntedBy -1<wu> 0<wu>

        if input.RightHeld && (HorizontalCentreOf world.Ship.ShipExtents) < ShipCentreRightmostX then
            world.Ship.ShipExtents <- world.Ship.ShipExtents |> RectangleShuntedBy 1<wu> 0<wu>

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
            b.BulletExtents <- b.BulletExtents |> RectangleShuntedBy 0<wu> -1<wu>

        let WhereBulletStillBelowTopmostPosition bullet =
            bullet.BulletExtents.TopW > BulletEndY

        let bulletsStillLive = world.Bullets |> List.filter WhereBulletStillBelowTopmostPosition   // TODO: optimise for case where all are on screen still

        bulletsStillLive |> List.iter ApplyUpwardMovementToBullet

        world.Bullets <- bulletsStillLive

    let ConsiderShotInvaders () =

        let deadBullets,deadInvaders = 
            CollisionsBetween 
                (world.Bullets |> WithAreasObtainedBy AreaOfBullet)
                (world.Invaders |> WithAreasObtainedBy AreaOfInvader)

        let scoreIncrease = (List.length deadInvaders) * ScoreForKillingInvader
        // TODO: explosions.

        let survingInvaders = world.Invaders |> List.filter (NotInList deadInvaders DogTagOfInvader)  // TODO: Prepare to return same list favouring no removals

        let survingBullets = world.Bullets |> List.filter (NotInList deadBullets AreaOfBullet)  // TODO: Prepare to return same list favouring no removals

        world.Bullets <- survingBullets
        world.Invaders <- survingInvaders
        IncreaseScoreBy scoreIncrease

    let ConsiderShotMothership () =

        // TODO: Performance optimise:  Don't do any of this if no motherships (a common case)

        let deadBullets,deadMotherships = 
            CollisionsBetween 
                (world.Bullets |> WithAreasObtainedBy AreaOfBullet)
                (world.Motherships |> WithAreasObtainedBy AreaOfMothership)

        let scoreIncrease = (List.length deadMotherships) * ScoreForKillingMothership
        // TODO: explosions.

        let survingMotherships = world.Motherships |> List.filter (NotInList deadMotherships AreaOfMothership)  // TODO: Prepare to return same list favouring no removals

        let survingBullets = world.Bullets |> List.filter (NotInList deadBullets AreaOfBullet)  // TODO: Prepare to return same list favouring no removals

        world.Bullets <- survingBullets
        world.Motherships <- survingMotherships
        IncreaseScoreBy scoreIncrease

    let MoveInvaders () =
    
        let (TickCount(ticks)) = timeNow
        if ticks &&& 7u = 0u then
            let ticks = ticks / 8u

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

    let ConsiderIntroducingMothership () =

        let elapsedTime = timeNow --- world.GameStartTime

        DoEvery TimeForMothershipCheck elapsedTime (fun () ->
            let x = MothershipCentreStartX - (MothershipWidth / 2)
            let newMothership = { MothershipExtents = { LeftW=x ; TopW=MotherShipTopY ; RightW=x+MothershipWidth ; BottomW=MotherShipTopY+MothershipHeight } }
            world.Motherships <- newMothership :: world.Motherships
        )

    let MoveMotherships () =

        let elapsedTime = timeNow --- world.GameStartTime

        DoEvery TimeForMothershipUpdate elapsedTime (fun () ->

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
        )

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
    ConsiderIntroducingMothership ()

    // TODO:  Release bombs
    // TODO:  Consider bombed ship or collided ship
    // TODO:  Consider if an invader has touched down on land == loss
    // TODO:  Move bombs and terminate
    // TODO:  We have no explosions!
    // TODO:  Scoring.

    if NoInvadersLeft () then 
        PlayerWon
    else if InvaderAtLowestLevel () then
        PlayerLost
    else
        GameContinuing


let HeadingAlignmentScore   = LeftAlign
let HeadingAlignmentHiScore = CentreAlign
let HeadingAlignmentLevel   = CentreAlign
let HeadingAlignmentLives   = RightAlign


[<Struct>]
type RenderActions =
    | DrawInvader    of l1:int<wu> * t1:int<wu> * dogTag:DogTag
    | DrawShip       of l2:int<wu> * t2:int<wu>
    | DrawBullet     of l3:int<wu> * t3:int<wu>
    | DrawMothership of l4:int<wu> * t4:int<wu>
    | ClearScreen
    | DrawText       of x:int<wu> * topY5:int<wu> * message:string * textAlign:TextAlignment


let BulletPositionOnTopOfShip theShip =

    let shipL = theShip.ShipExtents.LeftW
    let shipT = theShip.ShipExtents.TopW

    let bleft = shipL + ((ShipWidth - BulletWidth)/2)
    let btop  = shipT - BulletHeight

    (bleft,btop)
    

let RenderGamePlay renderer (gameWorld:GameWorld) =

    renderer (ClearScreen)

    gameWorld.Motherships |> List.iter 
        (fun motherShip -> 
            renderer (
                DrawMothership(
                    motherShip.MothershipExtents.LeftW,
                    motherShip.MothershipExtents.TopW)))

    gameWorld.Invaders |> List.iter
        (fun invader -> 
            renderer (
                DrawInvader(
                    invader.InvaderExtents.LeftW,
                    invader.InvaderExtents.TopW,
                    invader.DogTag)))

    let theShip = gameWorld.Ship
    let shipL = theShip.ShipExtents.LeftW
    let shipT = theShip.ShipExtents.TopW

    renderer (DrawShip(shipL, shipT))

    match theShip.WeaponReloadStartTimeOpt with
        | Some(_) -> ()
        | None    -> renderer (DrawBullet (BulletPositionOnTopOfShip theShip))

    gameWorld.Bullets |> List.iter
        (fun bullet -> 
            renderer (
                DrawBullet(
                    bullet.BulletExtents.LeftW,
                    bullet.BulletExtents.TopW)))

    let text x top message alignment =
        renderer (DrawText (x, top, message, alignment))

    let number x top (value:int) alignment =
        let s = value.ToString()
        renderer (DrawText (x, top, s, alignment))

    text   HeadingScoreX   ScoreboardTitlesTopY "SCORE"   HeadingAlignmentScore  
    text   HeadingHiScoreX ScoreboardTitlesTopY "HISCORE" HeadingAlignmentHiScore
    text   HeadingLevelX   ScoreboardTitlesTopY "LEVEL"   HeadingAlignmentLevel  
    text   HeadingLivesX   ScoreboardTitlesTopY "LIVES"   HeadingAlignmentLives  

    number HeadingScoreX   ScoreboardValuesTopY gameWorld.PlayStats.Score   HeadingAlignmentScore  
    number HeadingHiScoreX ScoreboardValuesTopY gameWorld.PlayStats.HiScore HeadingAlignmentHiScore
    number HeadingLevelX   ScoreboardValuesTopY gameWorld.PlayStats.Level   HeadingAlignmentLevel  
    number HeadingLivesX   ScoreboardValuesTopY gameWorld.PlayStats.Lives   HeadingAlignmentLives  


            