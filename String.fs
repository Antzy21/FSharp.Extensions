module FSharp.Extensions.String

open System
    
let ofSeq (charSeq: seq<char>) : string =
    charSeq |> String.Concat