module FSharp.Extensions.Array2D

let flaten (array: 'T[,]) : seq<'T> = 
    seq { for x in [0..(Array2D.length1 array) - 1] do 
                for y in [0..(Array2D.length2 array) - 1] do 
                    yield array.[x, y] }

let combineHorizontally (array1: 'T[,]) (array2: 'T[,]) : 'T[,] =
    if Array2D.length1 array1 <> Array2D.length1 array2 then
        failwith "Arrays must have the same height"
    else
        Array2D.init (Array2D.length1 array1) (Array2D.length2 array1 + Array2D.length2 array2) (fun i j ->
            if j < Array2D.length2 array1 then
                array1.[i, j]
            else
                array2.[i, j - Array2D.length2 array1]
        )
            
let combineVertically (array1: 'T[,]) (array2: 'T[,]) : 'T[,] =
    if Array2D.length2 array1 <> Array2D.length2 array2 then
        failwith "Arrays must have the same width"
    else
        Array2D.init (Array2D.length1 array1 + Array2D.length1 array2) (Array2D.length2 array1) (fun i j ->
            if i < Array2D.length1 array1 then
                array1.[i, j]
            else
                array2.[i - Array2D.length1 array1, j]
        )

let flateni (array: 'T[,]) : seq<('T * (int*int))> = 
    seq { for x in [0..(Array2D.length1 array) - 1] do 
                for y in [0..(Array2D.length2 array) - 1] do 
                    yield (array.[x, y], (x,y)) }

let fold (folder: 'S -> 'T -> 'S) (state: 'S) (array: 'T[,]) =
    let mutable state = state
    for x in 0 .. Array2D.length1 array - 1 do
        for y in 0 .. Array2D.length2 array - 1 do
            state <- folder state (array.[x, y])
    state

let filter (filterer: 'T -> bool) (array: 'T[,]) : 'T array =
    array
    |> fold (fun s item ->
        if filterer item then
            List.append s [item]
        else 
            s
    ) List.Empty
    |> Array.ofList

let foldij (folder: int -> int -> 'S -> 'T -> 'S) (state: 'S) (array: 'T[,]) =
    let mutable state = state
    for x in 0 .. Array2D.length1 array - 1 do
        for y in 0 .. Array2D.length2 array - 1 do
            state <- folder y x state (array.[x, y])
    state

let foldji (folder: int -> int -> 'S -> 'T -> 'S) (state: 'S) (array: 'T[,]) =
    let mutable state = state
    for y in 0 .. Array2D.length2 array - 1 do
        for x in 0 .. Array2D.length1 array - 1 do
            state <- folder x y state (array.[x, y])
    state

let foldjbacki (folder: int -> int -> 'S -> 'T -> 'S) (state: 'S) (array: 'T[,]) =
    let mutable state = state
    for y in ([0 .. Array2D.length2 array - 1] |> List.rev) do
        for x in 0 .. Array2D.length1 array - 1 do
            state <- folder x y state (array.[x, y])
    state

let foldibackj (folder: int -> int -> 'S -> 'T -> 'S) (state: 'S) (array: 'T[,]) =
    let mutable state = state
    for x in ([0 .. Array2D.length2 array - 1] |> List.rev) do
        for y in 0 .. Array2D.length2 array - 1 do
            state <- folder y x state (array.[x, y])
    state

let filterForPositions (filterer: 'T -> bool) (array: 'T[,]) : (int * int) array =
    array
    |> foldij (fun i j s item ->
        if filterer item then
            List.append s [(i, j)]
        else 
            s
    ) List.Empty
    |> Array.ofList

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
        
let tryFindi (predicate: int -> int -> 'T -> bool) (array: 'T[,]) =
    foldij (fun i j oValue t ->
        if Option.isSome oValue then
            oValue
        elif predicate i j t then
            Some t
        else
            None    
    ) None

let tryItem (i: int) (j: int) (array: 'T[,]) : 'T option =
    try 
        Some array.[i,j]
    with
    | _ -> None
    
let count (comparer: 'T -> bool) (array: 'T[,]) : int =
    array
    |> fold (fun s (t: 'T) ->
        if comparer t then
            s+1
        else
            s
    ) 0

let exists (comparer: 'T -> bool) (array: 'T[,]) : bool = count comparer array > 0

let last (array: 'T[,]) = array.[(Array2D.length1 array)-1, (Array2D.length2 array)-1]

/// Compares the value to the one left to it in the array using the comparison function. If there is no value to the left, take the default.
let leftComparison (defaultBool: bool) (comparison: 'T -> 'T -> bool) (i: int) (j: int) (array: 'T[,]) =
    if i = 0 then
        defaultBool
    else
        comparison array.[i-1,j] array.[i,j]
    
/// Compares the value to the one right to it in the array using the comparison function. If there is no value to the right, take the default.
let rightComparison (defaultBool: bool) (comparison: 'T -> 'T -> bool) (i: int) (j: int) (array: 'T[,]) =
    if i = Array2D.length1 array - 1 then
        defaultBool
    else
        comparison array.[i+1,j] array.[i,j]

/// Compares the value to the one above it in the array using the comparison function. If there is no value above, take the default.
let aboveComparison (defaultBool: bool) (comparison: 'T -> 'T -> bool) (i: int) (j: int) (array: 'T[,]) =
    if j = 0 then
        defaultBool
    else
        comparison array.[i,j-1] array.[i,j]

/// Compares the value to the one below it in the array using the comparison function. If there is no value below, take the default.
let belowComparison (defaultBool: bool) (comparison: 'T -> 'T -> bool) (i: int) (j: int) (array: 'T[,]) =
    if j = Array2D.length2 array - 1 then
        defaultBool
    else
        comparison array.[i,j+1] array.[i,j]

let adjacentComparisonCheck (array: 'T[,]) (comparison: 'T -> 'T -> bool) (i: int) (j: int) : bool=
    leftComparison true comparison i j array &&
    rightComparison true comparison i j array &&
    aboveComparison true comparison i j array &&
    belowComparison true comparison i j array
       
let adjacentFindi (array: 'T[,]) (predicate: 'T -> bool) (i: int) (j: int) =
    [|
        (tryItem (i-1) j array, ((i-1), j));
        (tryItem (i+1) j array, ((i+1), j));
        (tryItem i (j-1) array, (i, (j-1)));
        (tryItem i (j+1) array, (i, (j+1)));
    |]
    |> Array.filter (fst >> Option.isSome)
    |> Array.map (fun (o,ij) -> (Option.get o,ij))
    |> Array.find (fst >> predicate)

/// Compares adjacent values to find the max by the comparer
let adjacentComparer (array: 'T[,]) (i: int) (j: int) (comparer: 'T[] -> 'T) : 'T =
    [|
        tryItem (i-1) j array;
        tryItem (i+1) j array;
        tryItem i (j-1) array;
        tryItem i (j+1) array;
    |]
    |> Array.filter Option.isSome
    |> Array.map Option.get
    |> comparer

let modifyAdjacent
    (includeDiaganals: bool)
    (modifier: 'T -> 'T) 
    (i: int) (j: int) 
    (array: 'T[,])
    : 'T [,] =
    let X = Array2D.length1 array - 1
    let Y = Array2D.length2 array - 1
    if i <> 0 then
        if includeDiaganals then
            if j <> 0 then
                array.[i-1,j-1] <- (modifier array.[i-1,j-1])
            if j <> Y then
                array.[i-1,j+1] <- (modifier array.[i-1,j+1])
        array.[i-1,j] <- (modifier array.[i-1,j])
    if i <> X then
        if includeDiaganals then
            if j <> 0 then
                array.[i+1,j-1] <- (modifier array.[i+1,j-1])
            if j <> Y then
                array.[i+1,j+1] <- (modifier array.[i+1,j+1])
        array.[i+1,j] <- (modifier array.[i+1,j])
    if j <> 0 then
        array.[i,j-1] <- (modifier array.[i,j-1])
    if j <> Y then
        array.[i,j+1] <- (modifier array.[i,j+1])
    array

let print (array: 'a [,]) =
    printfn ""
    array
    |> Array2D.iteri (fun _ j cell ->
        match j = 0 with
        | true -> printf $"\n{cell}"
        | false -> printf $"{cell}"
    )

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

let printPass (array: 'a [,]) : 'a [,] = print array; array