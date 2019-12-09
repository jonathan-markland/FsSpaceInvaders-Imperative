module Geometry

// The game engine stores and calculates in World-Space, of type "int".
// The unit square is separately mapped to screen pixels by the renderer.


/// F# units-of-measure used to indicate "world" units.
[<Measure>] type wu


/// A cartesian point in World-Space.
type PointW =
    {
        xw:int<wu> ; yw:int<wu>
    }


/// A rectangle in World-Space.
type RectangleW =
    {
        LeftW:int<wu> ; TopW:int<wu> ; RightW:int<wu> ; BottomW:int<wu>
    }


let HorizontalCentreOf r = (r.LeftW + r.RightW) / 2
let VerticalCentreOf r   = (r.TopW + r.BottomW) / 2

let ShuntedBy dx dy r    =
    {
        LeftW    = r.LeftW   + dx
        TopW     = r.TopW    + dy
        RightW   = r.RightW  + dx
        BottomW  = r.BottomW + dy
    }

let ToCenteredRectangle (widthw:int<wu>) (heightw:int<wu>) {xw=xw ; yw=yw} =

    let dx = widthw / 2
    let dy = heightw / 2

    let left = xw - dx
    let top  = yw - dy

    {
        LeftW   = left
        TopW    = top
        RightW  = left + widthw
        BottomW = top + heightw
    }

let Intersects (r1:RectangleW) (r2:RectangleW) : bool =
    if r1.LeftW >= r2.RightW then false
    else if r1.RightW <= r2.LeftW then false
    else if r1.TopW >= r2.BottomW then false
    else if r1.BottomW <= r2.TopW then false
    else true

let HasListMemberThatIntersectsWith areaOfInstance getAreaOfListItem someList =
    someList |> List.tryFind (fun listItem -> areaOfInstance |> Intersects (listItem |> getAreaOfListItem)) |> Option.isSome

let CollisionsBetween (aList:'a list) (bList:'b list) getAreaOfA getAreaOfB =
    
    let intersectingAs = aList |> List.filter (fun a -> 
        let aArea = a |> getAreaOfA
        bList |> HasListMemberThatIntersectsWith aArea getAreaOfB)

    let intersectingBs = bList |> List.filter (fun b -> 
        let bArea = b |> getAreaOfB
        aList |> HasListMemberThatIntersectsWith bArea getAreaOfA)

    intersectingAs , intersectingBs

