module Screens

open GamePlayTypes
open InputEventData
open Mechanics
open GamePlay



let CalculateNextScreenState (currentState:Screen) (input:InputEventData) (timeNow:TickCount) =

    match currentState with
        
        | WelcomeScreen(lastHiScore) ->
            if input.FireJustPressed then
                GamePlayScreen (NewGameWorld 0 lastHiScore 3 1 timeNow)
            else
                currentState

        | GamePlayScreen(world) ->
            match CalculateNextFrameState world input timeNow with
                | PlayerWon      -> WelcomeScreen(world.PlayStats.HiScore)  // TODO: We should instead continue the next level.
                | PlayerLost     -> 
                    if world.PlayStats.Lives = 0 then
                        GameOverScreen(world.PlayStats.HiScore)
                    else
                        LifeOverScreen(world)
                | GameContinuing -> currentState

        | LifeOverScreen(world) ->
            if input.FireJustPressed then
                GamePlayScreen (world |> NextLifeGameWorld)
            else
                currentState

        | GameOverScreen(lastHiScore) ->
            if input.FireJustPressed then
                WelcomeScreen(lastHiScore)
            else
                currentState



let CompletelyNewGameStateWithResetHiScore () =
    let initialHiScore = 500
    WelcomeScreen(initialHiScore)
