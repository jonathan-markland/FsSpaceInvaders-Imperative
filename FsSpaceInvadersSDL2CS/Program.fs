
open SDL2
open System.IO

// Learn more about F# at https://fsharp.org
// See the 'F# Tutorial' project for more help.

[<Struct>]
type Window =
    {
        WindowHandle: nativeint
    }

[<Struct>]
type Surface =
    {
        SurfaceHandle: nativeint
    }

let UpdateWindowSurface {WindowHandle=h} =
    SDL.SDL_UpdateWindowSurface h |> ignore

let WithNewMainWindowDo windowTitleString windowWidth windowHeight operation =
    
    let window = 
        SDL.SDL_CreateWindow(
            windowTitleString, 
            100, 100, 
            windowWidth, windowHeight, 
            SDL.SDL_WindowFlags.SDL_WINDOW_SHOWN)

    if window = nativeint 0 then
        Error (sprintf "Window could not be created! SDL_Error: %s\n" (SDL.SDL_GetError ()))
    else
        try
            let operationResult = operation {WindowHandle = window}
            SDL.SDL_DestroyWindow(window)
            Ok (operationResult)
        with e ->
            Error (e.Message)

let WithWindowSurfaceDo operation {WindowHandle=wh} =

    let windowSurface = SDL.SDL_GetWindowSurface(wh)

    if windowSurface = 0n then
        Error (sprintf "Window surface could not be obtained! SDL_Error: %s\n" (SDL.SDL_GetError ()))
    else
        Ok (operation {SurfaceHandle = windowSurface})



[<Struct>]
type BMPImage =
    {
        BMPHandle: nativeint
    }

let LoadBMP filePath =
    let handle = SDL.SDL_LoadBMP(filePath)
    if handle = nativeint 0 then
        None
    else
        Some({ BMPHandle = handle })

type BMPSourceImage =
    {
        ImageHandle: BMPImage
        SourceRect:  SDL.SDL_Rect
    }

let ToSdlRect x y w h =
    let mutable r = SDL.SDL_Rect()
    r.x <- x
    r.y <- y
    r.w <- w
    r.h <- h
    r

let WithDimensions {BMPHandle=surface} =

    let t = typeof<SDL.SDL_Surface>
    let sur = (System.Runtime.InteropServices.Marshal.PtrToStructure(surface, t)) :?> SDL.SDL_Surface

    {
        ImageHandle = { BMPHandle=surface }
        SourceRect  = ToSdlRect 0 0 sur.w sur.h
    }

type SpaceInvadersBMPs =
    {
        Ship:        BMPSourceImage
        RedInvader:  BMPSourceImage
        BlueInvader: BMPSourceImage
        Bullet:      BMPSourceImage
    }

let LoadSpaceInvadersImages rootPath =

    let fromFile name = 
        let fullPath = Path.Combine (rootPath, name) + ".bmp"
        match LoadBMP fullPath with
            | Some(file) -> file |> WithDimensions
            | None       -> failwith (sprintf "Space invaders could not start because file '%s' is missing." fullPath)
    {
        Ship        = fromFile "Ship"
        RedInvader  = fromFile "RedInvader"
        BlueInvader = fromFile "BlueInvader"
        Bullet      = fromFile "Bullet"
    }

let DrawImage {SurfaceHandle=screenSurface} (image:BMPSourceImage) left top =
    let mutable dstRect = ToSdlRect left top image.SourceRect.w image.SourceRect.h
    let mutable srcRect = image.SourceRect
    SDL.SDL_BlitSurface (image.ImageHandle.BMPHandle, &srcRect, screenSurface, &dstRect) |> ignore



[<EntryPoint>]
let main argv =

    let imageSet = LoadSpaceInvadersImages ""

    let result = WithNewMainWindowDo "Space Invaders" 640 480 (fun mainWindow ->

        mainWindow |> WithWindowSurfaceDo (fun mainSurface ->

            let mutable quit = false
            while quit = false do

                let mutable e = new SDL.SDL_Event ()

                while (SDL.SDL_WaitEvent (&e)) <> 0 && not quit do   // SDL_PollEvent

                    let msg = e.``type``

                    if msg = SDL.SDL_EventType.SDL_QUIT then 
                        quit <- true

                    else if msg = SDL.SDL_EventType.SDL_MOUSEBUTTONDOWN then 
                        quit <- true

                    DrawImage mainSurface imageSet.Ship 100 100
                    DrawImage mainSurface imageSet.BlueInvader 170 100
                    DrawImage mainSurface imageSet.BlueInvader 200 100

                    UpdateWindowSurface mainWindow
        )
    )

    match result with
        | Error(message) ->
            printfn "%s" message
            1

        | Ok(_) ->
            0

