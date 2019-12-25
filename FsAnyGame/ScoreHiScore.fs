module ScoreHiScore

type ScoreAndHiScore =
    {
        Score:    uint32
        HiScore:  uint32
    }

let IncrementScoreBy n { Score=oldScore ; HiScore=oldHiScore } =

    let newScore = oldScore + n

    {
        Score    = newScore
        HiScore  = max newScore oldHiScore
    }
