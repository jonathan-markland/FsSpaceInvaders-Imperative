
open SDL2
open System.IO

// Learn more about F# at https://fsharp.org
// See the 'F# Tutorial' project for more help.

type Window =
    {
        WindowHandle: nativeint
    }

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

let DrawImage screenSurface (image:BMPSourceImage) left top =
    let mutable dstRect = ToSdlRect left top image.SourceRect.w image.SourceRect.h
    let mutable srcRect = image.SourceRect
    SDL.SDL_BlitSurface (image.ImageHandle.BMPHandle, &srcRect, screenSurface, &dstRect) |> ignore



[<EntryPoint>]
let main argv =

    let imageSet = LoadSpaceInvadersImages ""

    let window = SDL.SDL_CreateWindow("Space Invaders", 100, 100, 640, 480, SDL.SDL_WindowFlags.SDL_WINDOW_SHOWN)

    if window = nativeint 0 then

        printf "Window could not be created! SDL_Error: %s\n" (SDL.SDL_GetError ())
        1

    else

        let gScreenSurface = SDL.SDL_GetWindowSurface window

        if gScreenSurface = 0n then

            printf "Window surface could not be obtained! SDL_Error: %s\n" (SDL.SDL_GetError ())
            1

        else

            let mutable quit = false
            while quit = false do
            
                let mutable e = new SDL.SDL_Event ()

                while (SDL.SDL_WaitEvent (&e)) <> 0 && not quit do   // SDL_PollEvent

                    let msg = e.``type``

                    if msg = SDL.SDL_EventType.SDL_QUIT then 
                        quit <- true

                    else if msg = SDL.SDL_EventType.SDL_MOUSEBUTTONDOWN then 
                        quit <- true

                    DrawImage gScreenSurface imageSet.Ship 100 100
                    DrawImage gScreenSurface imageSet.BlueInvader 200 100

                    SDL.SDL_UpdateWindowSurface window |> ignore

            SDL.SDL_DestroyWindow window
            0

(*
	{
		//Get window surface
		screenSurface = SDL_GetWindowSurface(window);

		//Main loop flag
		bool quit = false;

		//While application is running
		while (!quit)
		{
			//Event handler
			SDL_Event e;

			//Handle events on queue
			while (SDL_WaitEvent(&e) != 0)   // SDL_PollEvent
			{
				//User requests quit
				if (e.type == SDL_QUIT)
				{
					quit = true;
					break;
				}
				else if (e.type == SDL_KEYDOWN)
				{
					MapHostKeyEventToAction(e.key.keysym.scancode, GetKeyPressAndMask, GetKeyPressOrMask, ReleaseMouseFromWindowIfEscape, KeyTranslationTableIndexedBySdlCode);
					break;
				}
				else if (e.type == SDL_KEYUP)
				{
					MapHostKeyEventToAction(e.key.keysym.scancode, GetKeyReleaseAndMask, GetKeyReleaseOrMask, DoNothingWithParameter<SDL_Scancode>, KeyTranslationTableIndexedBySdlCode);
					break;
				}
				else if (e.type == SDL_MOUSEBUTTONDOWN)
				{
					MapHostMouseButtonEventToAction(e.button.button, GetKeyPressAndMask, GetKeyPressOrMask, TieMouseToWindow, RemapSdlButtonEnumToBitMask);
					break;
				}
				else if (e.type == SDL_MOUSEBUTTONUP)
				{
					MapHostMouseButtonEventToAction(e.button.button, GetKeyReleaseAndMask, GetKeyReleaseOrMask, EmptyFunction, RemapSdlButtonEnumToBitMask);
					break;
				}
				else if (e.type == SDL_MOUSEMOTION)
				{
					MapHostMouseMotionEventToAction(e.motion.xrel, e.motion.yrel);
					break;
				}

				DrawSomething(screenSurface);

				//Update the surface
				SDL_UpdateWindowSurface(window);

				//Wait two seconds
				// SDL_Delay(200);
			}
		}
	}

	//Destroy window
	SDL_DestroyWindow(window);

	//Quit SDL subsystems
	SDL_Quit();

    printfn "%A" argv
    0 // return an integer exit code
*)
