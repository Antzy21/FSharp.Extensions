module FSharp.Extensions.Option

let failOnNone (message: string) = Option.defaultWith (fun () -> failwith message)