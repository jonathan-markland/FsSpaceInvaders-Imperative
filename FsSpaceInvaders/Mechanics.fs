module Mechanics

/// Timer ticks aka frame counts.
type TickCount = TickCount of uint32

type TickSpan = TickSpan of uint32

let (---) (TickCount(endTime)) (TickCount(startTime)) = 
    TickSpan(endTime - startTime)
