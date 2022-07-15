module FSharp.Extensions.Int

open System

let tryParse (str:string) =
  match System.Int32.TryParse str with
  | true, int -> Some int
  | _ -> None