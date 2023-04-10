module FSharp.Extensions.Seq

let foldResult (folder: 'State -> 'T -> 'State result) (state: 'State) (source: seq<'T>) : 'State result =
    Seq.fold (fun accResult item ->
        accResult
        |> Result.bind (fun acc ->
            folder acc item
        )
    ) (Ok state) source

let foldOption (folder: 'State -> 'T -> 'State option) (state: 'State) (source: seq<'T>) : 'State option =
    Seq.fold (fun accResult item ->
        accResult
        |> Option.bind (fun acc ->
            folder acc item
        )
    ) (Some state) source

let filterSome (source: 'T option seq) : 'T seq = 
    source
    |> Seq.filter Option.isSome
    |> Seq.map Option.get

let filterResults (source: 'T result seq) : 'T seq = 
    source
    |> Seq.map Result.toOption
    |> filterSome