module Geometry

// The game engine stores and calculates in Unit Square space, of type "float".
// The unit square is separately mapped to the screen by the renderer.
// Pixels are NOT used in the game engine!


/// A cartesian point in Unit Square Space, representing 
/// the centre of something.
type CentrePointD =
    {
        cx:float ; cy:float
    }


/// A rectangle in Unit Square Space.
type RectangleD =
    {
        Left:float ; Top:float ; Right:float ; Bottom:float
    }


let ToCenteredRectangle width height {cx=cx ; cy=cy} =

    let dx = width / 2.0
    let dy = height / 2.0

    let left = cx - dx
    let top  = cy - dy

    {
        Left   = left
        Top    = top
        Right  = left + width
        Bottom = top + height
    }


