module UnisonCloud.LogLine exposing (..)

import Dict exposing (Dict)
import Json.Decode as Decode
import Json.Decode.Pipeline exposing (required)
import UI.DateTime as DateTime exposing (DateTime)
import UUID exposing (UUID)
import UnisonCloud.LogLevel as LogLevel exposing (LogLevel)


type alias LogLineData =
    Dict String String


type alias LogLine =
    { id : UUID
    , loggedAt : DateTime
    , message : Maybe String
    , level : LogLevel
    , data : LogLineData
    }



-- HELPERS


hasData : LogLine -> Bool
hasData line =
    not (Dict.isEmpty line.data)


dataToList : LogLine -> List ( String, String )
dataToList line =
    Dict.toList line.data



-- DECODE


decode : Decode.Decoder LogLine
decode =
    let
        makeLine id loggedAt line =
            let
                message =
                    Dict.get "message" line

                level =
                    line
                        |> Dict.get "level"
                        |> Maybe.map LogLevel.fromString
                        |> Maybe.withDefault LogLevel.Info

                data =
                    line
                        |> Dict.remove "message"
                        |> Dict.remove "level"
            in
            { id = id
            , loggedAt = loggedAt
            , message = message
            , level = level
            , data = data
            }
    in
    Decode.succeed makeLine
        |> required "id" UUID.jsonDecoder
        |> required "time" DateTime.decode
        |> required "userMsg" (Decode.dict Decode.string)
