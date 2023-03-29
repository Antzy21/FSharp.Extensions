module FSharp.Extensions.List

let filterSome (source: 'T option list) : 'T list = 
    source
    |> List.filter Option.isSome
    |> List.map Option.get

let filterResults (source: 'T result list) : 'T list = 
    source
    |> List.map Result.toOption
    |> filterSome