module ScreenDrawing

open Dimensions
open Fonts
open DrawingCommands
open GameDrawing
open GamePlayTypes



let RenderWelcomeScreen render =
    let text x top message alignment =  // TODO: Move out?
        render (DrawText (x, top, message, alignment))
    render (TitleBackground)
    text   (ScreenWidth / 2) (ScreenHeight / 2) "SPACE INVADERS" CentreAlign  // TODO: Text not vertically centred in the screen.
    let y = (ScreenHeight * 6) / 7
    text   (ScreenWidth / 2) y "PRESS FIRE TO PLAY" CentreAlign  // TODO: Text not vertically centred in the screen.



let RenderGamePlayScreen render (gameWorld:GameWorld) =
    RenderGameWorld render gameWorld



let RenderNextLevelScreen render =
    let text x top message alignment =  // TODO: Move out?
        render (DrawText (x, top, message, alignment))
    render (NextLevelBackground)
    text   (ScreenWidth / 2) (ScreenHeight / 2) "NEXT LEVEL   WELL DONE" CentreAlign  // TODO: Text not vertically centred in the screen.
    


let RenderLifeOverScreen render =
    let text x top message alignment =  // TODO: Move out?
        render (DrawText (x, top, message, alignment))
    render (LifeOverBackground)
    text   (ScreenWidth / 2) (ScreenHeight / 2) "SHIP DESTROYED" CentreAlign  // TODO: Text not vertically centred in the screen.



let RenderGameOverScreen render =
    let text x top message alignment =  // TODO: Move out?
        render (DrawText (x, top, message, alignment))
    render (GameOverBackground)
    text   (ScreenWidth / 2) (ScreenHeight / 2) "GAME OVER" CentreAlign  // TODO: Text not vertically centred in the screen.



let RenderScreen render screen =
    match screen with
        | WelcomeScreen(_)          -> RenderWelcomeScreen render
        | GamePlayScreen(gameWorld) -> RenderGamePlayScreen render gameWorld
        | NextLevelScreen(_)        -> RenderNextLevelScreen render
        | LifeOverScreen(_)         -> RenderLifeOverScreen render
        | GameOverScreen(_)         -> RenderGameOverScreen render


