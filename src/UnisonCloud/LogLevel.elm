module UnisonCloud.LogLevel exposing (..)


type LogLevel
    = Info
    | Warn
    | Error
    | Custom String


fromString : String -> LogLevel
fromString raw =
    case String.toLower raw of
        "info" ->
            Info

        "warn" ->
            Warn

        "error" ->
            Error

        _ ->
            Custom raw
