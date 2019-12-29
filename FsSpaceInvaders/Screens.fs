module Screens

open GamePlayTypes
open InputEventData
open Mechanics
open GamePlay



let CalculateNextScreenState (currentState:Screen) (input:InputEventData) (timeNow:TickCount) =

    match currentState with
        
        | WelcomeScreen(lastHiScore) ->
            if input.FireJustPressed then
                GamePlayScreen (NewGameWorld 0u lastHiScore 3u 1u timeNow)
            else
                currentState

        | GamePlayScreen(world) ->
            match CalculateNextFrameState world input timeNow with
                | GameContinuing -> currentState
                | PlayerWon      -> NextLevelScreen(world |> NextLevelGameWorld)
                | PlayerLost     -> 
                    if world.PlayStats.Lives > 0u then
                        LifeOverScreen(world |> NextLifeGameWorld)
                    else
                        GameOverScreen(world.PlayStats.ScoreAndHiScore.HiScore)

        | NextLevelScreen(world) ->
            if input.FireJustPressed then
                GamePlayScreen(world)
            else
                currentState

        | LifeOverScreen(world) ->
            if input.FireJustPressed then
                GamePlayScreen(world)
            else
                currentState

        | GameOverScreen(lastHiScore) ->
            if input.FireJustPressed then
                WelcomeScreen(lastHiScore)
            else
                currentState



let CompletelyNewGameStateWithResetHiScore () =
    let initialHiScore = 500u
    WelcomeScreen(initialHiScore)
