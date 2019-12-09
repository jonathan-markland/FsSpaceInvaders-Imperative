module Mechanics

/// Timer ticks aka frame counts.
type TickCount = TickCount of uint32

type TickSpan = TickSpan of uint32

let (---) (TickCount(endTime)) (TickCount(startTime)) = 
    TickSpan(endTime - startTime)

/// Evaluate whether the game elapsed time is an exact multiple
/// of the frequency given, and return f() if so.
let Every (frequency:TickSpan) (elapsedTime:TickSpan) f =
    let (TickSpan(freq)) = frequency
    let (TickSpan(elapsed)) = elapsedTime
    if (elapsed % freq) = 0u then Some(f ()) else None

/// Evaluate whether the game elapsed time is an exact multiple
/// of the frequency given, and invoke f() if so.
let DoEvery frequency elapsedTime f =
    Every frequency elapsedTime f |> ignore
