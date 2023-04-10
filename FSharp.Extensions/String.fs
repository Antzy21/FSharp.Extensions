module FSharp.Extensions.String

open System
    
/// Concatenate a sequence of chars into a string
let ofSeq (charSeq: seq<char>) : string =
    charSeq |> String.Concat

/// Takes a string and wraps it onto new lines such that the number of lines equals the length of each line
let toBlock (str: string) : string =
    let size = int(Math.Sqrt(str.Length))
    str
    |> Seq.rev
    |> Seq.chunkBySize size
    |> Seq.fold (fun s chunk ->
        s + (String.Concat chunk) + "\n"
    ) ""