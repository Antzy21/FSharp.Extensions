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
    let failOnError (result: 'T result) = Result.defaultWith failwith