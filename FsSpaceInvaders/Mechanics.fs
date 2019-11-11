module Mechanics

type TickCount = TickCount of int

/// Calculate position offset given the start and end positions,
/// time at start, time now, and movement total duration.
/// using linear interpolation.
let CalculatePositionLinearly
    (positionAtStart:float)
    (positionAtEnd:float)
    (startTime:TickCount) 
    (totalDuration:TickCount) 
    (timeNow:TickCount) =

    let (TickCount(startTime))     = startTime
    let (TickCount(totalDuration)) = totalDuration
    let (TickCount(timeNow))       = timeNow

    let dt = timeNow - startTime

    if dt >= 0 && dt <= totalDuration then
        let positionRange = positionAtEnd - positionAtStart
        let offset = float dt / float totalDuration
        Some(positionRange * offset + positionAtStart)
    else
        None

