module Screens

open GamePlayTypes
open InputEventData
open Mechanics
open GamePlay



let CalculateNextScreenState (currentState:Screen) (input:InputEventData) (timeNow:TickCount) =

    match currentState with
        
        | WelcomeScreen(lastHiScore) ->
            if input.FireJustPressed then
                let freshWorld = NewGameWorld lastHiScore timeNow
                GamePlayScreen freshWorld
            else
                currentState

        | GamePlayScreen(world) ->
            match CalculateNextFrameState world input timeNow with
                | PlayerWon      -> WelcomeScreen(world.PlayStats.HiScore)  // TODO: We should instead continue the next level.
                | PlayerLost     -> GameOverScreen(world.PlayStats.HiScore)
                | GameContinuing -> currentState

        | GameOverScreen(lastHiScore) ->
            if input.FireJustPressed then
                WelcomeScreen(lastHiScore)
            else
                currentState



let CompletelyNewGameStateWithResetHiScore () =
    let initialHiScore = 500
    WelcomeScreen(initialHiScore)
