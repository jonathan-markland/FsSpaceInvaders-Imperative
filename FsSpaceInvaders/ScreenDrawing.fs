module ScreenDrawing

open Dimensions
open Fonts
open DrawingCommands
open GameDrawing
open GamePlayTypes



let private text x top message alignment =
    DrawText (x, top, message, alignment)    // TODO: Text not vertically centred in the screen.



let RenderWelcomeScreen render =
    render (TitleBackground)
    render (text (ScreenWidth / 2) (ScreenHeight / 2)       "SPACE INVADERS"     CentreAlign)
    render (text (ScreenWidth / 2) ((ScreenHeight * 6) / 7) "PRESS FIRE TO PLAY" CentreAlign)



let RenderGamePlayScreen render gameWorld =
    RenderGameWorld render gameWorld



let RenderNextLevelScreen render =
    render (NextLevelBackground)
    render (text (ScreenWidth / 2) (ScreenHeight / 2) "NEXT LEVEL   WELL DONE" CentreAlign)
    


let RenderLifeOverScreen render =
    render (LifeOverBackground)
    render (text (ScreenWidth / 2) (ScreenHeight / 2) "SHIP DESTROYED" CentreAlign)



let RenderGameOverScreen render =
    render (GameOverBackground)
    render (text (ScreenWidth / 2) (ScreenHeight / 2) "GAME OVER" CentreAlign)



let RenderScreen render screen =
    match screen with
        | WelcomeScreen(_)          -> RenderWelcomeScreen render
        | GamePlayScreen(gameWorld) -> RenderGamePlayScreen render gameWorld
        | NextLevelScreen(_)        -> RenderNextLevelScreen render
        | LifeOverScreen(_)         -> RenderLifeOverScreen render
        | GameOverScreen(_)         -> RenderGameOverScreen render


