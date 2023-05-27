module FSharp.Extensions.Array

let filterSome (source: 'T option array) : 'T array = 
    source
    |> Array.filter Option.isSome
    |> Array.map Option.get

let filterResults (source: 'T result array) : 'T array = 
    source
    |> Array.map Result.toOption
    |> filterSome