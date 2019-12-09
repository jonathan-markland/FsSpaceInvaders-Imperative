﻿open SDL2
open System.IO
open SDLCover
open GamePlay
open GamePlayTypes
open Rules
open Mechanics
open InputEventData
open Geometry


type SpaceInvadersBMPs =
    {
        Ship:        BMPSourceImage
        RedInvader:  BMPSourceImage
        BlueInvader: BMPSourceImage
        Bullet:      BMPSourceImage
        Mothership:  BMPSourceImage
        Font:        BMPSourceImage
    }



let LoadSpaceInvadersImages rootPath =

    let fromFile name = 
        let fullPath = Path.Combine(Path.Combine(rootPath, "Images"), name) + ".bmp"
        match LoadBMP fullPath with
            | Some(file) -> file |> WithDimensions
            | None       -> failwith (sprintf "Space invaders could not start because file '%s' is missing." fullPath)
    {
        Ship        = fromFile "Ship"
        RedInvader  = fromFile "RedInvader"
        BlueInvader = fromFile "BlueInvader"
        Bullet      = fromFile "Bullet"
        Mothership  = fromFile "Mothership"
        Font        = fromFile "Font"
    }



let RenderToSdlSurface imageSet fontDefinition targetSurface renderAction =

    /// Convert World Units <wu> to our pixels.  It happens that this is 1:1 with 256 x 256 schemes.
    let px (n:int<wu>) = int n

    match renderAction with
        
        | DrawBullet(left,top) ->
            DrawImage targetSurface imageSet.Bullet (px left) (px top)

        | DrawInvader(left,top,dogTag) ->
            let invaderBmp =
                match (InvaderColourFromDogTag dogTag) with
                    | RedInvader  -> imageSet.RedInvader
                    | BlueInvader -> imageSet.BlueInvader
            DrawImage targetSurface invaderBmp (px left) (px top)

        | DrawMothership(left,top) ->
            DrawImage targetSurface imageSet.Mothership (px left) (px top)

        | DrawShip(left,top) ->
            DrawImage targetSurface imageSet.Ship (px left) (px top)

        | ClearScreen ->
            DrawFilledRectangle targetSurface 0 0 256 256 0u
            
        | DrawText(x,top,message,textAlign) ->
            DrawTextString targetSurface (px x) (px top) message textAlign fontDefinition



let TimerCallback (interval:uint32) (param:nativeint) : uint32 =

    let mutable event = new SDL.SDL_Event()

    event.``type`` <- SDL.SDL_EventType.SDL_USEREVENT
    event.user.code <- 0
    event.user.data1 <- 0n
    event.user.data2 <- 0n

    SDL.SDL_PushEvent(&event) |> ignore
    1u  // We can return 0u to cancel the timer here, or non-zero to keep it going.




[<EntryPoint>]
let main argv =

    let initResult = SDL.SDL_Init(SDL.SDL_INIT_TIMER)
    if initResult <> 0 then
        failwith "Failed to initialise SDL."

    let imageSet = LoadSpaceInvadersImages ""
    let fontDefinition = MakeFont imageSet.Font.ImageHandle

    let hiScore = 1000 // hack
    let timeNowTickCount = TickCount(0u) // hack
    let gamePlayState = NewGameWorld hiScore timeNowTickCount

    let result = WithNewMainWindowDo "Space Invaders" 256 256 (fun mainWindow ->

        mainWindow |> WithWindowSurfaceDo (fun mainSurface ->

            let timerID = SDL.SDL_AddTimer(15u,new SDL.SDL_TimerCallback(TimerCallback),0n)
            if timerID = 0 then
                failwith "Failed to install the gameplay timer."

            let renderFunction = (RenderToSdlSurface imageSet fontDefinition mainSurface)

            let mutable leftHeld = false
            let mutable rightHeld = false
            let mutable tickCount = 0u

            let mutable fireJustPressed = false  // until discovered otherwise
            let mutable fireWaitingRelease = false

            let mutable quit = false
            while quit = false do

                let mutable event = new SDL.SDL_Event ()

                while (SDL.SDL_WaitEvent (&event)) <> 0 && not quit do   // SDL_PollEvent

                    let msg = event.``type``

                    if msg = SDL.SDL_EventType.SDL_QUIT then 
                        quit <- true

                    else if msg = SDL.SDL_EventType.SDL_MOUSEBUTTONDOWN then 
                        quit <- true

                    else if msg = SDL.SDL_EventType.SDL_KEYDOWN then
                        match event.key.keysym.scancode with
                            | SDL.SDL_Scancode.SDL_SCANCODE_LEFT  -> leftHeld <- true
                            | SDL.SDL_Scancode.SDL_SCANCODE_RIGHT -> rightHeld <- true
                            | SDL.SDL_Scancode.SDL_SCANCODE_Z     -> 
                                if fireWaitingRelease 
                                then () 
                                else 
                                    fireJustPressed <- true
                                    fireWaitingRelease <- true
                            | _ -> ()

                    else if msg = SDL.SDL_EventType.SDL_KEYUP then
                        match event.key.keysym.scancode with
                            | SDL.SDL_Scancode.SDL_SCANCODE_LEFT  -> leftHeld <- false
                            | SDL.SDL_Scancode.SDL_SCANCODE_RIGHT -> rightHeld <- false
                            | SDL.SDL_Scancode.SDL_SCANCODE_Z     -> fireWaitingRelease <- false
                            | _ -> ()

                    else if msg = SDL.SDL_EventType.SDL_USEREVENT then
                        // ~ This is the AddTimer event handler 
                        tickCount <- tickCount + 1u
                        // gamePlayState |> ApplyInputsToGamePlayState  tickCount
                        let inputEventData = { LeftHeld=leftHeld ; RightHeld=rightHeld ; FireJustPressed=fireJustPressed }
                        CalculateNextFrameState gamePlayState inputEventData (TickCount(tickCount)) |> ignore // TODO: sort out return code handling
                        RenderGamePlay renderFunction gamePlayState
                        UpdateWindowSurface mainWindow
                        fireJustPressed <- false
        ) 
    )

    match result with
        | Error(message) ->
            printfn "%s" message
            1

        | Ok(_) ->
            0

