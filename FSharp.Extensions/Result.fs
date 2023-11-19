namespace FSharp.Extensions

/// Wrapper for Result type where the error type is a string, in the style of the option type.
type 'T result = Result<'T, string>

module Result =
    
    /// Converts an option type into a result type, with a given string error message. 
    let fromOption (errorMessage: string) (option: 'T option) : 'T result =
        match option with
        | Some result -> Ok result
        | None -> Error errorMessage

    /// If an Error, throws an exception with the error message
    let failOnError (result: 'T result) : 'T =
        result |> Result.defaultWith failwith
    
    // Combines if statements with result types. When a function returns a 'bool result' instead of just a 'bool' to wrap in an 'if func() then', use this.
    let elseif (condition: unit -> bool result) (resultValue: 'T) (value: 'T option) : 'T option result =
        match value with
        | Some value -> Ok (Some value)
        | None ->
            condition ()
            |> Result.map (fun value ->
                if value then
                    (Some resultValue)
                else
                    None
            )

    // Same as Option.orElseWith but for results.
    let orElseWith (ifNoneThunk: unit -> Result<'T, 'TError>) (result: Result<'T, 'TError>) : Result<'T, 'TError> =
        match result with
        | Error _ -> ifNoneThunk ()
        | okResult -> okResult
