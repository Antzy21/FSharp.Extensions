module FSharp.Extensions.Option

/// If None, throws an exception with a given error message
let failOnNone (errorMessage: string) = Option.defaultWith (fun () -> failwith errorMessage)