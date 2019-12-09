module Algorithm

/// Returns true if 'item' does not exist in 'theList', where keySelector is
/// a function that returns a comparison key, given the item.
let NotInList theList keySelector item =
    
    let itemToFindKey = item |> keySelector
    
    theList 
        |> List.tryFind (fun listItem -> 
            let thisItemKey = listItem |> keySelector
            itemToFindKey = thisItemKey) 

        |> Option.isNone

