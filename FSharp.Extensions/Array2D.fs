module FSharp.Extensions.Array2D

type coordinates = coordinates<int>

/// Are some given coordinates are inside a given array
let containsCoordinates ((i,j): coordinates) (array: 'T[,]) : bool =
    i >= 0 && i < Array2D.length1 array && j >= 0 && j < Array2D.length2 array

/// Checks if coordinates are in array, then apply given modifier to array at coordinatess if they are.
let modifyArrayAtCoordinates (modifier: 'T -> 'T) ((i,j): coordinates) (array: 'T[,]) =
    if containsCoordinates (i, j) array then
        array.[i,j] <- (modifier array.[i,j])

/// For a list of coordinates, check if they are in array, then apply given modifier to array at coordinatess if they are.
let modifyArrayAtListOfCoords (modifier: 'T -> 'T)  (array: 'T[,]) (coordsList: coordinates seq) =
    coordsList
    |> Seq.iter (fun coords ->
        modifyArrayAtCoordinates modifier coords array
    )

/// Convert a array into a sequence
let flaten (array: 'T[,]) : 'T seq = 
    seq {
        for i in [0..(Array2D.length1 array) - 1] do 
            for j in [0..(Array2D.length2 array) - 1] do 
                yield array.[i, j]
    }
    
/// Convert a array into a sequence of tuples containging the values and their i, j coordinates.
let flateni (array: 'T[,]) : ('T * (int*int)) seq = 
    seq {
        for i in [0..(Array2D.length1 array) - 1] do 
            for j in [0..(Array2D.length2 array) - 1] do 
                yield (array.[i, j], (i,j))
    }

/// For two given arrays which must have the same height, combine them next to eachother with A2 to the right of A1.
let combineHorizontallyResult (array1: 'T[,]) (array2: 'T[,]) : 'T[,] result =
    if Array2D.length1 array1 <> Array2D.length1 array2 then
        Error "Arrays must have the same height"
    else
        Array2D.init (Array2D.length1 array1) (Array2D.length2 array1 + Array2D.length2 array2) (fun i j ->
            if j < Array2D.length2 array1 then
                array1.[i, j]
            else
                array2.[i, j - Array2D.length2 array1]
        )
        |> Ok

/// For two given arrays which must have the same height, combine them next to eachother with A2 to the right of A1.
let combineHorizontallyOption (array1: 'T[,]) (array2: 'T[,]) = combineHorizontallyResult array1 array2 |> Result.toOption

/// For two given arrays which must have the same height, combine them next to eachother with A2 to the right of A1.
let combineHorizontally (array1: 'T[,]) (array2: 'T[,]) = combineHorizontallyResult array1 array2 |> Result.failOnError
            
/// For two given arrays which must have the same width, combine them next to eachother with A2 below A1.
let combineVerticallyResult (array1: 'T[,]) (array2: 'T[,]) : 'T[,] result =
    if Array2D.length2 array1 <> Array2D.length2 array2 then
        Error "Arrays must have the same width"
    else
        Array2D.init (Array2D.length1 array1 + Array2D.length1 array2) (Array2D.length2 array1) (fun i j ->
            if i < Array2D.length1 array1 then
                array1.[i, j]
            else
                array2.[i - Array2D.length1 array1, j]
        )
        |> Ok

/// For two given arrays which must have the same width, combine them next to eachother with A2 below A1.
let combineVerticallyOption (array1: 'T[,]) (array2: 'T[,]) = combineVerticallyResult array1 array2 |> Result.toOption

/// For two given arrays which must have the same width, combine them next to eachother with A2 below A1.
let combineVertically (array1: 'T[,]) (array2: 'T[,]) = combineVerticallyResult array1 array2 |> Result.failOnError

/// Flattens the array and then folds it.
let fold (folder: 'State -> 'T -> 'State) (state: 'State) (array: 'T[,]) : 'State =
    flaten array |> Seq.fold folder state

/// Flattens the array and then reduces it
let reduce (reduction: 'T -> 'T -> 'T) (array: 'T[,]) : 'T =
    flaten array |> Seq.reduce reduction

/// Returns a list of all elements in a given array that meet the criteria of a filtering function.
let filter (filterer: 'T -> bool) (array: 'T[,]) : 'T seq =
    flaten array |> Seq.filter filterer

/// Folds the array, starting in the top left and moving right.
let foldij (folder: coordinates -> 'S -> 'T -> 'S) (state: 'S) (array: 'T[,]) =
    [0..Array2D.length1 array-1]
    |> List.fold (fun accRow i ->
        [0..Array2D.length2 array-1]
        |> List.fold (fun acc j ->
            array.[i, j]
            |> folder (i, j) acc
        ) accRow
    ) state
    
/// Folds the array, starting in the bottom left and moving right.
let foldijback (folder: coordinates -> 'S -> 'T -> 'S) (state: 'S) (array: 'T[,]) =
    [0..Array2D.length1 array-1] |> List.rev
    |> List.fold (fun accRow i ->
        [0..Array2D.length2 array-1]
        |> List.fold (fun acc j ->
            array.[i, j]
            |> folder (i, j) acc
        ) accRow
    ) state

/// Folds the array, starting in the top right and moving left.
let foldibackj (folder: coordinates -> 'S -> 'T -> 'S) (state: 'S) (array: 'T[,]) =
    [0..Array2D.length1 array-1]
    |> List.fold (fun accRow i ->
        [0..Array2D.length2 array-1] |> List.rev
        |> List.fold (fun acc j ->
            array.[i, j]
            |> folder (i, j) acc
        ) accRow
    ) state

/// Folds the array, starting in the bottom right and moving left.
let foldibackjback (folder: coordinates -> 'S -> 'T -> 'S) (state: 'S) (array: 'T[,]) =
    [0..Array2D.length1 array-1] |> List.rev
    |> List.fold (fun accRow i ->
        [0..Array2D.length2 array-1] |> List.rev
        |> List.fold (fun acc j ->
            array.[i, j]
            |> folder (i, j) acc
        ) accRow
    ) state

/// Folds the array, starting in the top left and moving down.
let foldji (folder: coordinates -> 'S -> 'T -> 'S) (state: 'S) (array: 'T[,]) =
    [0..Array2D.length1 array-1]
    |> List.fold (fun accRow j ->
        [0..Array2D.length2 array-1]
        |> List.fold (fun acc i ->
            array.[i, j]
            |> folder (i, j) acc
        ) accRow
    ) state
    
/// Folds the array, starting in the top right and moving down.
let foldjiback (folder: coordinates -> 'S -> 'T -> 'S) (state: 'S) (array: 'T[,]) =
    [0..Array2D.length1 array-1] |> List.rev
    |> List.fold (fun accRow j ->
        [0..Array2D.length2 array-1]
        |> List.fold (fun acc i ->
            array.[i, j]
            |> folder (i, j) acc
        ) accRow
    ) state
    
/// Folds the array, starting in the bottom left and moving up.
let foldjbacki (folder: coordinates -> 'S -> 'T -> 'S) (state: 'S) (array: 'T[,]) =
    [0..Array2D.length1 array-1]
    |> List.fold (fun accRow j ->
        [0..Array2D.length2 array-1] |> List.rev
        |> List.fold (fun acc i ->
            array.[i, j]
            |> folder (i, j) acc
        ) accRow
    ) state
    
/// Folds the array, starting in the bottom right and moving up.
let foldjbackiback (folder: coordinates -> 'S -> 'T -> 'S) (state: 'S) (array: 'T[,]) =
    [0..Array2D.length1 array-1] |> List.rev
    |> List.fold (fun accRow j ->
        [0..Array2D.length2 array-1] |> List.rev
        |> List.fold (fun acc i ->
            array.[i, j]
            |> folder (i, j) acc
        ) accRow
    ) state

/// Get a list of coordinates from an array that satisfy a function on the values in the array.
let filterForCoordinates (filterer: 'T -> bool) (array: 'T[,]) : coordinates array =
    array
    |> foldij (fun coords s item ->
        if filterer item then
            coords :: s
        else 
            s
    ) []
    |> Array.ofList
    
/// Try to find a value in an array using a filtering function.
let tryFind (predicate: 'T -> bool) (array: 'T[,]) =
    array
    |> fold (fun oValue t ->
        if Option.isSome oValue then
            oValue
        elif predicate t then
            Some t
        else
            None
    ) None

/// Try to find the coordinates of a value that fits a given predicate in an arry
let tryFindIndex (predicate: 'T -> bool) (array: 'T[,]) : coordinates option =
    array
    |> foldij (fun coords oCoords t ->
        if Option.isSome oCoords then
            oCoords
        elif predicate t then
            Some coords
        else
            None
    ) None
  
/// Try to find a value in an array using a filtering function involving coordinates.
let tryFindi (predicate: coordinates -> 'T -> bool) (array: 'T[,]) : 'T option =
    array
    |> foldij (fun coords oValue t ->
        if Option.isSome oValue then
            oValue
        elif predicate coords t then
            Some t
        else
            None    
    ) None

/// Try to get the value in an array at given coordinates.
let tryGet ((i,j): coordinates) (array: 'T[,]) : 'T option =
    if containsCoordinates (i,j) array then
        Some array.[i,j]
    else
        None

/// Get the last element in the bottom right of the array
let last (array: 'T[,]) =
    array.[(Array2D.length1 array)-1, (Array2D.length2 array)-1]

/// For two given coordinates, check if they are in a given array, and then compare the two with a given comparison function.
let compareAtCoordinatesResult (comparison: 'T -> 'T -> bool) ((i1, j1): coordinates) ((i2, j2): coordinates) (array: 'T[,]) =
    if not <| containsCoordinates (i1, j1) array then
        Error $"Coordinates ({i1}, {j1}) are not in the array"
    else if not <| containsCoordinates (i2, j2) array then
        Error $"Coordinates ({i2}, {j2}) are not in the array"
    else
        comparison array.[i1,j1] array.[i2,j2]
        |> Ok

/// For two given coordinates, check if they are in a given array, and then compare the two with a given comparison function.
let compareAtCoordinatesOption (comparison: 'T -> 'T -> bool) (coords1: coordinates) (coords2: coordinates) (array: 'T[,]) =
    compareAtCoordinatesResult comparison coords1 coords2 array |> Result.toOption

/// For two given coordinates, check if they are in a given array, and then compare the two with a given comparison function.
let compareAtCoordinates (comparison: 'T -> 'T -> bool) (coords1: coordinates) (coords2: coordinates) (array: 'T[,]) =
    compareAtCoordinatesResult comparison coords1 coords2 array |> Result.failOnError

/// Compare a value in an array at given coordinates with orthogonal adjacent values, using a comparison function.
let orthogonallyAdjacentComparisonCheckResult (array: 'T[,]) (comparison: 'T -> 'T -> bool) (coords: coordinates) : bool =
    Coordinates.getOrthogonalAdjacentCoordinates coords
    |> Seq.fold (fun accumulatedCheck adjCoords ->
        compareAtCoordinatesOption comparison coords adjCoords array
        |> Option.defaultValue true
        |> ((&&) accumulatedCheck)
    ) true
       
/// Compares adjacent values to find the max by the comparer
let adjacentComparer (array: 'T[,]) (coords: coordinates) (comparer: 'T seq -> 'T) : 'T =
    Coordinates.getAdjacentCoordinates coords
    |> Seq.map (fun adjCoords -> tryGet adjCoords array)
    |> Seq.filterSome
    |> comparer

/// Modify the values of orthoganaly adjacent values in an array
let modifyOrthognallyAdjacent (modifier: 'T -> 'T) (coords: coordinates) (array: 'T[,]) : 'T [,] =
    Coordinates.getOrthogonalAdjacentCoordinates coords
    |> modifyArrayAtListOfCoords modifier array
    array
    
/// Modify the values of diagonally adjacent values in an array
let modifyDiagonallyAdjacent (modifier: 'T -> 'T) (coords: coordinates) (array: 'T[,]) : 'T [,] =
    Coordinates.getDiagonallyAdjacentCoordinates coords
    |> modifyArrayAtListOfCoords modifier array
    array

/// Modify the values of adjacent values in an array
let modifyAdjacent (modifier: 'T -> 'T) (coords: coordinates) (array: 'T[,]) : 'T [,] =
    Coordinates.getAdjacentCoordinates coords
    |> modifyArrayAtListOfCoords modifier array
    array

/// Prints out each element of the cell in an uneven matrix, but preserving vertical lines.
let print (array: 'a [,]) =
    printfn ""
    array
    |> Array2D.iteri (fun _ j cell ->
        match j = 0 with
        | true -> printf $"\n{cell}"
        | false -> printf $"{cell}"
    )

/// Prints out an integer array after values have been "moduloed" by 10.
let printMod10 (array: int [,]) =
    printfn ""
    array
    |> Array2D.iteri (fun _ j cell ->
        let cell = 
            $"{cell % 10}"

        match j = 0 with
        | true -> printf $"\n{cell}"
        | false -> printf $"{cell}"
    )

/// Prints out an array of bools, with true represented with "X" and false ".".
let printBool (array: bool [,]) =
    printfn ""
    array
    |> Array2D.iteri (fun _ j cell ->
        let cell =
            match cell with
            | true -> "X"
            | false -> "."

        match j = 0 with
        | true -> printf $"\n{cell}"
        | false -> printf $"{cell}"
    )

/// Prints out the array to the console, and returns it to continue operations.
let printPass (array: 'a [,]) : 'a [,] = print array; array