/// A cover for some things in the the SDL2CS, to give stronger typing from the F# viewpoint.
module SDLCover

open SDL2
open Fonts


let ToSdlRect x y w h =
    let mutable r = SDL.SDL_Rect()
    r.x <- x
    r.y <- y
    r.w <- w
    r.h <- h
    r



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

let WithDimensions {BMPHandle=surface} =

    let t = typeof<SDL.SDL_Surface>
    let s = (System.Runtime.InteropServices.Marshal.PtrToStructure(surface, t)) :?> SDL.SDL_Surface

    {
        ImageHandle = { BMPHandle=surface }
        SourceRect  = ToSdlRect 0 0 s.w s.h
    }



type FontDefinition =
    {
        FontImageHandle:  BMPImage
        CharWidth:        int
        CharHeight:       int
    }


let MakeFont bmpImage =
    {
        FontImageHandle = bmpImage
        CharWidth  = 6  // TODO
        CharHeight = 8  // TODO
    }







[<Struct>]
type Window =
    {
        WindowHandle: nativeint
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



[<Struct>]
type Surface =
    {
        SurfaceHandle: nativeint
    }

let WithWindowSurfaceDo operation {WindowHandle=wh} =

    let windowSurface = SDL.SDL_GetWindowSurface(wh)

    if windowSurface = 0n then
        Error (sprintf "Window surface could not be obtained! SDL_Error: %s\n" (SDL.SDL_GetError ()))
    else
        Ok (operation {SurfaceHandle = windowSurface})



/// Draw a BMPSourceImage onto a surface at a given position.
let DrawImage {SurfaceHandle=screenSurface} (image:BMPSourceImage) left top =
    let mutable dstRect = ToSdlRect left top image.SourceRect.w image.SourceRect.h
    let mutable srcRect = image.SourceRect
    SDL.SDL_BlitSurface (image.ImageHandle.BMPHandle, &srcRect, screenSurface, &dstRect) |> ignore

/// Draw part of a BMPImage onto a surface at a given position.
let DrawSubImage {SurfaceHandle=screenSurface} (imageHandle:BMPImage) srcleft srctop srcwidth srcheight dstleft dsttop dstwidth dstheight =
    let mutable dstRect = ToSdlRect dstleft dsttop dstwidth dstheight
    let mutable srcRect = ToSdlRect srcleft srctop srcwidth srcheight
    SDL.SDL_BlitSurface (imageHandle.BMPHandle, &srcRect, screenSurface, &dstRect) |> ignore

/// Draw a filled rectangle onto the surface at given position in given colour
let DrawFilledRectangle {SurfaceHandle=screenSurface} left top right bottom fillColour =
    let mutable rect = ToSdlRect left top (right-left) (bottom-top)
    SDL.SDL_FillRect (screenSurface, &rect, fillColour) |> ignore

/// Draw text at given position in given font, with given alignment.
let DrawTextString targetSurface x top message textAlign (fontDefinition:FontDefinition) =

    let cwd = fontDefinition.CharWidth
    let cht = fontDefinition.CharHeight
    let bmp = fontDefinition.FontImageHandle

    let measuredWidth (s:string) =
        s.Length * cwd

    let mutable posx =
        match textAlign with
            | LeftAlign   -> x
            | CentreAlign -> x - (message |> measuredWidth) / 2
            | RightAlign  -> x - (message |> measuredWidth)

    message |> Seq.iter (fun ch -> 
        let write charIndex = DrawSubImage targetSurface bmp  (charIndex * cwd) 0 cwd cht  posx top cwd cht
        if      ch >= '0' && ch <= '9' then write ((int ch) - 48)
        else if ch >= 'A' && ch <= 'Z' then write ((int ch) - 55)
        else if ch >= 'a' && ch <= 'z' then write ((int ch) - 87)
        else ()
        posx <- posx + cwd
    )
