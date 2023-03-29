module FSharp.Extensions.Console

open System

/// Display message to user, then wait for input.
/// Attempt to parse input into 'T to return.
/// Reprompt for new input on failed parsing.
let rec ParseLine (message: string) (func : string -> 'T option) : 'T =
    printfn $"{message}"
    match Console.ReadLine() |> func with
    | None -> ParseLine message func
    | Some parsedValue -> parsedValue

/// Display message to user, then wait for input.
/// If input is empty, return None, otherwise, attempt to parse input into 'T to return.
/// Reprompt for new input on failed parsing.
let rec ParseLineWithBreakOption (message: string) (func : string -> 'T option) : 'T option =
    printfn $"{message}"
    match Console.ReadLine() with
    | "" -> None
    | input ->
        match func input with
        | None -> ParseLineWithBreakOption message func
        | Some parsedValue -> Some parsedValue