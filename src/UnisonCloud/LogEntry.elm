module UnisonCloud.LogEntry exposing (..)

import Dict exposing (Dict)
import Json.Decode as Decode
import Json.Decode.Pipeline exposing (required)
import UI.DateTime as DateTime exposing (DateTime)
import UnisonCloud.LogLevel as LogLevel exposing (LogLevel)


type alias LogEntry =
    { loggedAt : DateTime
    , message : Maybe String
    , level : LogLevel
    , data : Dict String String
    }



-- DECODE


decode : Decode.Decoder LogEntry
decode =
    let
        makeEntry loggedAt entry =
            let
                message =
                    Dict.get "message" entry

                level =
                    entry
                        |> Dict.get "level"
                        |> Maybe.map LogLevel.fromString
                        |> Maybe.withDefault LogLevel.Info

                data =
                    entry
                        |> Dict.remove "message"
                        |> Dict.remove "level"
            in
            { loggedAt = loggedAt
            , message = message
            , level = level
            , data = data
            }
    in
    Decode.succeed makeEntry
        |> required "time" DateTime.decode
        |> required "userMsg" (Decode.dict Decode.string)
