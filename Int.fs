module FSharp.Extensions.Int

/// Converts the string representation of a number to its 32-bit signed integer equivalent. The return value is an optional int.
let tryParse (str:string) =
  match System.Int32.TryParse str with
  | true, int -> Some int
  | _ -> None