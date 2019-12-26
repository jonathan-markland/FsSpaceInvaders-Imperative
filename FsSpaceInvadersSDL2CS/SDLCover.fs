/// A cover for some things in the the SDL2CS, to give stronger typing from the F# viewpoint.
module SDLCover

// TODO:  https://stackoverflow.com/questions/33402909/sdl-renderpresent-vs-sdl-updatewindowsurface

(*
If you store your images in RAM and use the CPU for rendering(this is called software rendering) you use SDL_UpdateWindowSurface.
By calling this function you tell the CPU to update the screen and draw using software rendering.
You can store your textures in RAM by using SDL_Surface, but software rendering is inefficent. You can give draw calls by using SDL_BlitSurface.
SDL_UpdateWindowSurface is equivalent to the SDL 1.2 API SDL_Flip().
On the other side when you use the GPU to render textures and you store your texture on the GPU(this is called hardware accelerated rendering), which you should, you use SDL_RenderPresent.
This function tells the GPU to render to the screen.
You store texture on the GPU using SDL_Texture. When using this you can give draw calls by using SDL_RenderCopy or if you want transformations SDL_RenderCopyEx
Therefore, when using SDL's rendering API, one does all drawing intended for the frame, and then calls this function once per frame to present the final drawing to the user.
You should you use hardware rendering it's far more efficent, than software rendering! Even if the user running the program hasn't got a GPU (which is rare, because most CPU's have an integrated GPU) SDL will switch to software rendering by it self!
By the way you can load an image as SDL_Texture without the need to load an image as an SDL_Surface and convert it to an SDL_Texture using the SDL_image library, which you should because it supports several image formats not just BMP, like pure SDL. (SDL_image is made by the creators of SDL)
Just use the IMG_LoadTexture from SDL_image!

share
improve this answer
edited Oct 29 '15 at 11:07 


answered Oct 29 '15 at 10:37 

kovacsmarcell 
2111
1 silver badge
8
8 bronze badges
 
 *)

open SDL2
open Fonts

[<Struct>]
type WindowNativeInt =
    {
        WindowNativeInt: nativeint
    }

[<Struct>]
type BMPNativeInt =
    {
        BMPNativeInt: nativeint
    }

[<Struct>]
type SurfaceNativeInt =
    {
        SurfaceNativeInt: nativeint
    }

[<Struct>]
type TextureNativeInt =
    {
        TextureNativeInt: nativeint
    }

[<Struct>]
type RendererNativeInt =
    {
        RendererNativeInt: nativeint
    }



let CreateWindowAndRenderer width height =

    let mutable windowNativeInt = 0n
    let mutable rendererNativeInt = 0n

    let createWRResult =
        SDL.SDL_CreateWindowAndRenderer(
            width,
            height,
            SDL.SDL_WindowFlags.SDL_WINDOW_RESIZABLE,
            &windowNativeInt,
            &rendererNativeInt)

    if createWRResult = 0 then
        Some({ WindowNativeInt=windowNativeInt } , { RendererNativeInt=rendererNativeInt })
    else
        None



let ToSdlRect x y w h =

    let mutable r = SDL.SDL_Rect()
    r.x <- x
    r.y <- y
    r.w <- w
    r.h <- h
    r



let LoadBMP filePath =
    let handle = SDL.SDL_LoadBMP(filePath)
    if handle = nativeint 0 then
        None
    else
        Some({ BMPNativeInt = handle })



type BMPSourceImage =
    {
        ImageHandle:   BMPNativeInt
        TextureHandle: TextureNativeInt
        SourceRect:    SDL.SDL_Rect
    }

let BMPImagePreparedForRenderer rendererNativeInt { BMPNativeInt = bmp } =

    let t = typeof<SDL.SDL_Surface>
    let s = (System.Runtime.InteropServices.Marshal.PtrToStructure(bmp, t)) :?> SDL.SDL_Surface

    let texture = SDL.SDL_CreateTextureFromSurface(rendererNativeInt,bmp)
    if texture <> 0n then
        Some({
            ImageHandle   = { BMPNativeInt=bmp }
            TextureHandle = { TextureNativeInt=texture }
            SourceRect    = ToSdlRect 0 0 s.w s.h
        })
    else
        None



type FontDefinition =
    {
        FontImageHandle:  BMPNativeInt
        CharWidth:        int
        CharHeight:       int
    }


let MakeFont bmpImage =
    {
        FontImageHandle = bmpImage
        CharWidth  = 6  // TODO
        CharHeight = 8  // TODO
    }







let UpdateWindowSurface {WindowNativeInt=h} =
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
            let operationResult = operation {WindowNativeInt = window}
            SDL.SDL_DestroyWindow(window)
            Ok (operationResult)
        with e ->
            Error (e.Message)



let WithWindowSurfaceDo operation {WindowNativeInt=wh} =

    let windowSurface = SDL.SDL_GetWindowSurface(wh)

    if windowSurface = 0n then
        Error (sprintf "Window surface could not be obtained! SDL_Error: %s\n" (SDL.SDL_GetError ()))
    else
        Ok (operation {SurfaceNativeInt = windowSurface})



/// Draw a BMPSourceImage onto a surface at a given position.
let DrawImage {RendererNativeInt=renderer} (image:BMPSourceImage) left top =
    let mutable dstRect = ToSdlRect left top image.SourceRect.w image.SourceRect.h
    let mutable srcRect = image.SourceRect
    // SDL.SDL_BlitSurface (image.ImageHandle.BMPNativeInt, &srcRect, screenSurface, &dstRect) |> ignore
    SDL.SDL_RenderCopy(renderer, image.TextureHandle.TextureNativeInt, &srcRect, &dstRect) |> ignore

/// Draw part of a BMPImage onto a surface at a given position.
let DrawSubImage {RendererNativeInt=renderer} (imageHandle:BMPNativeInt) srcleft srctop srcwidth srcheight dstleft dsttop dstwidth dstheight =
    let mutable dstRect = ToSdlRect dstleft dsttop dstwidth dstheight
    let mutable srcRect = ToSdlRect srcleft srctop srcwidth srcheight
    // SDL.SDL_BlitSurface (imageHandle.BMPNativeInt, &srcRect, screenSurface, &dstRect) |> ignore
    SDL.SDL_RenderCopy(renderer, imageHandle.BMPNativeInt, &srcRect, &dstRect) |> ignore

/// Draw a filled rectangle onto the surface at given position in given colour
let DrawFilledRectangle {RendererNativeInt=renderer} left top right bottom (colourRGB:uint32) =
    let mutable rect = ToSdlRect left top (right-left) (bottom-top)
    // SDL.SDL_FillRect (screenSurface, &rect, fillColour) |> ignore
    SDL.SDL_SetRenderDrawColor(
        renderer, 
        uint8 (colourRGB >>> 16),
        uint8 (colourRGB >>> 8),
        uint8 colourRGB,
        0xFFuy) |> ignore
    SDL.SDL_RenderFillRect(renderer, &rect) |> ignore

/// Draw text at given position in given font, with given alignment.
let DrawTextString renderer x top message textAlign (fontDefinition:FontDefinition) =

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
        let write charIndex = DrawSubImage renderer bmp  (charIndex * cwd) 0 cwd cht  posx top cwd cht
        if      ch >= '0' && ch <= '9' then write ((int ch) - 48)
        else if ch >= 'A' && ch <= 'Z' then write ((int ch) - 55)
        else if ch >= 'a' && ch <= 'z' then write ((int ch) - 87)
        else ()
        posx <- posx + cwd
    )



let WithSdl2Do f =
    try
        let initResult = SDL.SDL_Init(SDL.SDL_INIT_TIMER)
        if initResult = 0 then
            Some(f ())
        else
            None
    with 
        | :? System.BadImageFormatException ->
            None



let Present {RendererNativeInt=renderer} =
    SDL.SDL_RenderPresent(renderer)


    