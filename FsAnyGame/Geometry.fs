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



let RectangleCenteredAboutPoint (width:int<wu>) (height:int<wu>) {xw=centrex ; yw=centrey} =

    let halfwidth  = width / 2
    let halfheight = height / 2

    let left = centrex - halfwidth
    let top  = centrey - halfheight

    {
        LeftW   = left
        TopW    = top
        RightW  = left + width
        BottomW = top  + height
    }



let RectangleIntersects (r1:RectangleW) (r2:RectangleW) : bool =

    if r1.LeftW >= r2.RightW then false
    else if r1.RightW <= r2.LeftW then false
    else if r1.TopW >= r2.BottomW then false
    else if r1.BottomW <= r2.TopW then false
    else true



[<Struct>]
type RegionObjectsList<'a> =
    {
        RegionObjectsList: 'a list
        RegionObjectItemAreaGetter: 'a -> RectangleW
    }



let WithAreasObtainedBy areaGetterFunction someList =
    {
        RegionObjectsList = someList
        RegionObjectItemAreaGetter = areaGetterFunction
    }



let HasListMemberThatIntersectsWith areaOfInstance regionObjectList =
    let { RegionObjectsList = someList ; RegionObjectItemAreaGetter = getAreaOfListItem } = regionObjectList
    someList 
        |> List.tryFind (fun listItem -> areaOfInstance |> RectangleIntersects (listItem |> getAreaOfListItem)) 
        |> Option.isSome



let CollisionsBetween objectListA objectListB =
    
    let { RegionObjectsList = aList ; RegionObjectItemAreaGetter = getAreaOfA } = objectListA
    let { RegionObjectsList = bList ; RegionObjectItemAreaGetter = getAreaOfB } = objectListB

    let intersectingAs = aList |> List.filter (fun a -> 
        let aArea = a |> getAreaOfA
        objectListB |> HasListMemberThatIntersectsWith aArea)

    let intersectingBs = bList |> List.filter (fun b -> 
        let bArea = b |> getAreaOfB
        objectListA |> HasListMemberThatIntersectsWith bArea)

    intersectingAs , intersectingBs

