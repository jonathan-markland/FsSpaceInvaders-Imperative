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
open ScoreHiScore

// TODO:  Speed up as number of invaders reduces.
// TODO:  Invader step downwards should be larger.  Timing of their advance needs revisiting.
// TODO:  How could we change the game each level?



let NewInvaderPack () =

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

    [for y in 1..InvaderRowsCount do
        for x in 1..InvadersPerRow do
            NewInvader x y]



let ShipInLevelStartPosition () =

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



/// Create a fresh game world for a new game.
let NewGameWorld hiScore (timeNow:TickCount) : GameWorld =

    {
        GameStartTime = timeNow

        PlayStats =
            {
                Level   = 1u
                ScoreAndHiScore = { Score=0u ; HiScore=hiScore }
                Lives   = 3u
            }

        Motherships  = []
        Invaders     = NewInvaderPack ()
        Bullets      = []
        Bombs        = []
        Explosions   = []
        Ship         = ShipInLevelStartPosition ()
        PlayEndedYet = None
    }



/// Given a world where the player has just lost a life,
/// return a new world for the next life.
let NextLifeGameWorld (outgoing:GameWorld) : GameWorld =

    let oldStats = outgoing.PlayStats

    {
        GameStartTime = outgoing.GameStartTime
        PlayStats     = { oldStats with Lives = oldStats.Lives - 1u }
        Motherships   = outgoing.Motherships
        Invaders      = outgoing.Invaders
        Bullets       = []
        Bombs         = []
        Explosions    = []
        Ship          = outgoing.Ship
        PlayEndedYet  = None
    }



/// Given a world where the player has just won the level,
/// return a new world for the next level.
let NextLevelGameWorld (outgoing:GameWorld) : GameWorld =

    let oldStats   = outgoing.PlayStats
    let newScoring = oldStats.ScoreAndHiScore |> IncrementScoreBy ScoreForNextLevel

    {
        GameStartTime = outgoing.GameStartTime
        PlayStats     = { oldStats with Lives = oldStats.Lives + 1u ; Level = oldStats.Level + 1u ; ScoreAndHiScore = newScoring }
        Motherships   = []
        Invaders      = NewInvaderPack ()
        Bullets       = []
        Bombs         = []
        Explosions    = []
        Ship          = ShipInLevelStartPosition ()
        PlayEndedYet  = None
    }



type FrameResult = GameContinuing | PlayerWon | PlayerLost



let BulletPositionOnTopOfShip theShip =

    let shipL = theShip.ShipExtents.LeftW
    let shipT = theShip.ShipExtents.TopW

    let bleft = shipL + ((ShipWidth - BulletWidth)/2)
    let btop  = shipT - BulletHeight

    (bleft,btop)



let NewBulletFiredFromCentrallyAbove theShip =

    let x,y = BulletPositionOnTopOfShip theShip

    {
        BulletExtents =
            {
                LeftW    = x
                RightW   = x + BulletWidth
                TopW     = y
                BottomW  = y + BulletHeight
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

    let RandomInvader () =  // TODO: This exhibits really bad gameplay

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

        let newValue = world.PlayStats.ScoreAndHiScore |> IncrementScoreBy n
        world.PlayStats.ScoreAndHiScore <- newValue

    let MoveShip () =

        if input.LeftHeld && (HorizontalCentreOf world.Ship.ShipExtents) > ShipCentreLeftmostX then
            world.Ship.ShipExtents <- world.Ship.ShipExtents |> RectangleShuntedBy -ShipMovementStep 0<wu>

        if input.RightHeld && (HorizontalCentreOf world.Ship.ShipExtents) < ShipCentreRightmostX then
            world.Ship.ShipExtents <- world.Ship.ShipExtents |> RectangleShuntedBy ShipMovementStep 0<wu>

    let ConsiderBulletFiring () =

        let ConsiderReloadPenalty () =

            match world.Ship.WeaponReloadStartTimeOpt with
                | Some(startTime) -> 
                    if (timeNow --- startTime) >= TimeForReloadShipWeapon then
                        world.Ship.WeaponReloadStartTimeOpt <- None
                | None -> ()

        let CheckFireButton () =

            if input.FireJustPressed && world.Ship.WeaponReloadStartTimeOpt |> Option.isNone then
                let newBullet = NewBulletFiredFromCentrallyAbove world.Ship
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

    let MoveBullets () =

        let ApplyUpwardMovementToBullet b =
            b.BulletExtents <- b.BulletExtents |> RectangleShuntedBy 0<wu> -BulletStep

        let WhereBulletStillBelowTopmostPosition bullet =
            bullet.BulletExtents.TopW > BulletEndY

        let bulletsStillInPlay = 
            world.Bullets |> List.filter WhereBulletStillBelowTopmostPosition   // TODO: optimise for case where all are on screen still

        bulletsStillInPlay |> List.iter ApplyUpwardMovementToBullet

        world.Bullets <- bulletsStillInPlay

    let MoveBombs () =

        let ApplyDownwardMovementToBomb b =
            b.BombExtents <- b.BombExtents |> RectangleShuntedBy 0<wu> BombStep

        let WhereBombStillAboveFloorPosition bomb =
            bomb.BombExtents.BottomW < BombFloorY

        let bombsStillInPlay = 
            world.Bombs |> List.filter WhereBombStillAboveFloorPosition   // TODO: optimise for case where all are on screen still

        bombsStillInPlay |> List.iter ApplyDownwardMovementToBomb

        world.Bombs <- bombsStillInPlay

    let WithAdditionalExplosionsFor listOfThings areaOfThing preExistingExplosions =

        preExistingExplosions |> List.append

            (listOfThings |> List.map (fun t ->
                {
                    ExplosionExtents = areaOfThing t
                    StartTime        = timeNow
                }
            ))

    let ConsiderShotInvaders () =

        let deadBullets,deadInvaders = 
            CollisionsBetweenLists 
                (world.Bullets  |> WithAreasObtainedBy AreaOfBullet)
                (world.Invaders |> WithAreasObtainedBy AreaOfInvader)

        let scoreIncrease = uint32 (List.length deadInvaders) * ScoreForKillingInvader

        let survingInvaders = world.Invaders |> List.filter (NotInList deadInvaders DogTagOfInvader)  // TODO: Prepare to return same list favouring no removals

        let survingBullets = world.Bullets |> List.filter (NotInList deadBullets AreaOfBullet)  // TODO: Prepare to return same list favouring no removals

        let newExplosionsState = world.Explosions |> WithAdditionalExplosionsFor deadInvaders AreaOfInvader 

        world.Bullets <- survingBullets
        world.Invaders <- survingInvaders
        world.Explosions <- newExplosionsState
        IncreaseScoreBy scoreIncrease

    let ConsiderShotMothership () =

        // TODO: Performance optimise:  Don't do any of this if no motherships (a common case)

        let deadBullets,deadMotherships = 
            CollisionsBetweenLists 
                (world.Bullets |> WithAreasObtainedBy AreaOfBullet)
                (world.Motherships |> WithAreasObtainedBy AreaOfMothership)

        let scoreIncrease = uint32 (List.length deadMotherships) * ScoreForKillingMothership

        let survingMotherships = world.Motherships |> List.filter (NotInList deadMotherships AreaOfMothership)  // TODO: Prepare to return same list favouring no removals

        let survingBullets = world.Bullets |> List.filter (NotInList deadBullets AreaOfBullet)  // TODO: Prepare to return same list favouring no removals

        let sumTotalExplosions = world.Explosions |> WithAdditionalExplosionsFor deadMotherships AreaOfMothership

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
    
        let (TickCount(ticks)) = timeNow   // TODO: Measure from the start of the screen?

        let dx = if (ticks &&& 16u)  = 0u then 1<wu> else -1<wu>   // TODO:  Use % with tunable constants
        let dy = if (ticks &&& 255u) = 0u then 8<wu> else 0<wu>    // TODO:  Use % with tunable constants

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

        let dx = MothershipStep

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

    let LevelOver () =
        InvaderAtLowestLevel ()
        || ShipCollidedWithInvader ()
        || ShipCollidedWithBomb ()

    match world.PlayEndedYet with

        | None ->
            MoveShip ()
            ConsiderBulletFiring ()
            ConsiderDroppingBombs ()
            MoveBullets ()
            MoveBombs ()
            ConsiderShotInvaders ()
            ConsiderShotMothership ()
            MoveInvaders ()
            MoveMotherships ()
            ConsiderIntroducingMothership ()
            ConsiderRemovingExplosions ()

            world.PlayEndedYet <- 
                if NoInvadersLeft () then 
                    Some(timeNow, EndBecauseWon)
                else if LevelOver () then
                    ExplodeTheShip ()
                    Some(timeNow, EndBecauseLost)
                else
                    None

            GameContinuing

        | Some(endedAt,reason) ->

            let elapsedInEndState = timeNow --- endedAt

            if elapsedInEndState < TimeForEndState then

                MoveBullets ()
                MoveBombs ()
                ConsiderShotInvaders ()
                ConsiderShotMothership ()
                MoveInvaders ()
                MoveMotherships ()
                ConsiderRemovingExplosions ()

                GameContinuing

            else
                match reason with
                    | EndBecauseWon  -> PlayerWon
                    | EndBecauseLost -> PlayerLost
                    
