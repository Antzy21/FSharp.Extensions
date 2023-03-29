module FSharp.Extensions.String

open System
    
/// Concatenate a sequence of chars into a string
let ofSeq (charSeq: seq<char>) : string =
    charSeq |> String.Concat