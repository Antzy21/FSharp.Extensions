module FSharp.Extensions.Console

open System

let rec ParseLine (message: string) (func : string -> 'T option) : 'T =
    printfn $"{message}"
    match Console.ReadLine() |> func with
    | None -> ParseLine message func
    | Some parsedValue -> parsedValue

let rec ParseLineWithBreakOption (message: string) (func : string -> 'T option) : 'T option =
    printfn $"{message}"
    match Console.ReadLine() with
    | "" -> None
    | input ->
        match func input with
        | None -> ParseLineWithBreakOption message func
        | Some parsedValue -> Some parsedValue