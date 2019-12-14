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
open DrawingCommands


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

        Bombs =
            []

        Explosions =
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



let NewBombPositionedCentrallyUnder someRectangle =

    let leftSide = (HorizontalCentreOf someRectangle) - (BombWidth / 2)
    let topY    = someRectangle.BottomW

    {
        BombExtents =
            {
                LeftW    = leftSide
                RightW   = leftSide + BombWidth
                TopW     = topY 
                BottomW  = topY + BombHeight
            }
    }




let CalculateNextFrameState (world:GameWorld) (input:InputEventData) (timeNow:TickCount) =

    let elapsedTime = timeNow --- world.GameStartTime

    let RandomInvader () =

        let invadersList = world.Invaders
        match invadersList with
            | [] ->
                None
            | _  ->
                let countLeft = invadersList |> List.length
                let (TickCount(ticks)) = timeNow
                let selectedIndex = int (ticks % uint32 countLeft)
                Some(invadersList |> List.item selectedIndex)

    let IncreaseScoreBy n =

        world.PlayStats.Score <- world.PlayStats.Score + n
        if world.PlayStats.Score > world.PlayStats.HiScore then
            world.PlayStats.HiScore <- world.PlayStats.Score

    let MoveShip () =

        if input.LeftHeld && (HorizontalCentreOf world.Ship.ShipExtents) > ShipCentreLeftmostX then
            world.Ship.ShipExtents <- world.Ship.ShipExtents |> RectangleShuntedBy -1<wu> 0<wu>

        if input.RightHeld && (HorizontalCentreOf world.Ship.ShipExtents) < ShipCentreRightmostX then
            world.Ship.ShipExtents <- world.Ship.ShipExtents |> RectangleShuntedBy 1<wu> 0<wu>

    let ConsiderBulletFiring () =

        let ConsiderReloadPenalty () =

            match world.Ship.WeaponReloadStartTimeOpt with
                | Some(startTime) -> 
                    if (timeNow --- startTime) >= TimeForReloadShipWeapon then
                        world.Ship.WeaponReloadStartTimeOpt <- None
                | None -> ()

        let CheckFireButton () =

            if input.FireJustPressed && world.Ship.WeaponReloadStartTimeOpt |> Option.isNone then
                let newBullet = NewBulletFiredFromCentrallyAbove world.Ship.ShipExtents
                let updatedBulletList = newBullet :: world.Bullets
                world.Bullets <- updatedBulletList
                world.Ship.WeaponReloadStartTimeOpt <- Some(timeNow)

        ConsiderReloadPenalty ()
        CheckFireButton ()

    let ConsiderDroppingBombs () =

        DoEvery TimeForNewBombCheck elapsedTime (fun () ->
            let firingInvader = RandomInvader ()
            match firingInvader with
                | None -> ()
                | Some(firingInvader) -> 
                    let newBomb = NewBombPositionedCentrallyUnder (firingInvader |> AreaOfInvader)
                    let updateBombsList = newBomb :: world.Bombs
                    world.Bombs <- updateBombsList
                    ()
        )

    let UpdateBullets () =

        let ApplyUpwardMovementToBullet b =
            b.BulletExtents <- b.BulletExtents |> RectangleShuntedBy 0<wu> -1<wu>

        let WhereBulletStillBelowTopmostPosition bullet =
            bullet.BulletExtents.TopW > BulletEndY

        let bulletsStillLive = 
            world.Bullets |> List.filter WhereBulletStillBelowTopmostPosition   // TODO: optimise for case where all are on screen still

        bulletsStillLive |> List.iter ApplyUpwardMovementToBullet

        world.Bullets <- bulletsStillLive

    let UpdateBombs () =

        DoEvery TimeForBombUpdateCheck elapsedTime (fun () ->

            let ApplyDownwardMovementToBomb b =
                b.BombExtents <- b.BombExtents |> RectangleShuntedBy 0<wu> 1<wu>

            let WhereBombStillAboveFloorPosition bullet =
                bullet.BombExtents.BottomW < BombFloorY

            let bombsStillAlive = 
                world.Bombs |> List.filter WhereBombStillAboveFloorPosition   // TODO: optimise for case where all are on screen still

            bombsStillAlive |> List.iter ApplyDownwardMovementToBomb

            world.Bombs <- bombsStillAlive
        )

    let ExplosionsForAll listOfThings areaOfThing worldExplosions =

        worldExplosions |> List.append

            (listOfThings |> List.map (fun t ->
                {
                    ExplosionExtents = areaOfThing t
                    StartTime        = timeNow
                }
            ))

    let ConsiderShotInvaders () =

        let deadBullets,deadInvaders = 
            CollisionsBetween 
                (world.Bullets |> WithAreasObtainedBy AreaOfBullet)
                (world.Invaders |> WithAreasObtainedBy AreaOfInvader)

        let scoreIncrease = (List.length deadInvaders) * ScoreForKillingInvader

        let survingInvaders = world.Invaders |> List.filter (NotInList deadInvaders DogTagOfInvader)  // TODO: Prepare to return same list favouring no removals

        let survingBullets = world.Bullets |> List.filter (NotInList deadBullets AreaOfBullet)  // TODO: Prepare to return same list favouring no removals

        let sumTotalExplosions = ExplosionsForAll deadInvaders AreaOfInvader world.Explosions

        world.Bullets <- survingBullets
        world.Invaders <- survingInvaders
        world.Explosions <- sumTotalExplosions
        IncreaseScoreBy scoreIncrease

    let ConsiderShotMothership () =

        // TODO: Performance optimise:  Don't do any of this if no motherships (a common case)

        let deadBullets,deadMotherships = 
            CollisionsBetween 
                (world.Bullets |> WithAreasObtainedBy AreaOfBullet)
                (world.Motherships |> WithAreasObtainedBy AreaOfMothership)

        let scoreIncrease = (List.length deadMotherships) * ScoreForKillingMothership

        let survingMotherships = world.Motherships |> List.filter (NotInList deadMotherships AreaOfMothership)  // TODO: Prepare to return same list favouring no removals

        let survingBullets = world.Bullets |> List.filter (NotInList deadBullets AreaOfBullet)  // TODO: Prepare to return same list favouring no removals

        let sumTotalExplosions = ExplosionsForAll deadMotherships AreaOfMothership world.Explosions

        world.Bullets <- survingBullets
        world.Motherships <- survingMotherships
        world.Explosions <- sumTotalExplosions
        IncreaseScoreBy scoreIncrease

    let ConsiderRemovingExplosions () =

        let survivingExplosions = world.Explosions |> List.filter (fun e ->     // TODO: Prepare to return same list favouring no removals
            let elapsedSinceExplosionStarted = timeNow --- e.StartTime
            elapsedSinceExplosionStarted < TimeForWholeExplosion)

        world.Explosions <- survivingExplosions

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

    let ShipCollidedWithInvader () =

        let shipRect = world.Ship.ShipExtents
        let collidedWithShip invader = invader.InvaderExtents |> RectangleIntersects shipRect
        world.Invaders |> List.exists (fun invader -> invader |> collidedWithShip)

    let ShipCollidedWithBomb () =

        let shipRect = world.Ship.ShipExtents
        let collidedWithShip bomb = bomb.BombExtents |> RectangleIntersects shipRect
        world.Bombs |> List.exists (fun bomb -> bomb |> collidedWithShip)

    let ExplodeTheShip () =

        let shipExplosion = 
            {
                ExplosionExtents = world.Ship.ShipExtents
                StartTime = timeNow
            }

        let newExplosionsList =
            shipExplosion :: world.Explosions

        world.Explosions <- newExplosionsList

    MoveShip ()
    ConsiderBulletFiring ()
    ConsiderDroppingBombs ()
    UpdateBullets ()
    UpdateBombs ()
    ConsiderShotInvaders ()
    ConsiderShotMothership ()
    MoveInvaders ()
    MoveMotherships ()
    ConsiderIntroducingMothership ()
    ConsiderRemovingExplosions ()

    // TODO: Require carrying on for a few frames post-ship-destruction.

    let LevelOver () =
        InvaderAtLowestLevel ()
        || ShipCollidedWithInvader ()
        || ShipCollidedWithBomb ()

    if NoInvadersLeft () then 
        PlayerWon
    else if LevelOver () then
        ExplodeTheShip ()
        PlayerLost
    else
        GameContinuing





            