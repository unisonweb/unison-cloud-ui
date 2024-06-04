module UnisonCloud.LogLine exposing (..)

import Dict exposing (Dict)
import Json.Decode as Decode
import Json.Decode.Extra exposing (doubleEncoded)
import Json.Decode.Pipeline exposing (required)
import UI.DateTime as DateTime exposing (DateTime)
import UnisonCloud.LogLevel as LogLevel exposing (LogLevel)


type alias LogLineData =
    Dict String String


type alias LogLine =
    { id : String
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

        decodeUserValue =
            Decode.oneOf
                [ Decode.string
                , Decode.map String.fromInt Decode.int
                ]
    in
    Decode.succeed makeLine
        |> required "id" Decode.string
        |> required "time" DateTime.decode
        |> required "userMsg" (Decode.dict decodeUserValue)


decodeList : List String -> List LogLine
decodeList raw =
    let
        {-
           TODO: We've encountered exactly 1 unparseable (via JSON.parse in JS
           land and thus Elm) log line, so for now if we encounter lines like
           that, we don't let them break the whole page, but remove them from the
           list of logs
        -}
        decodeItem item acc =
            case Decode.decodeString decode item of
                Ok l ->
                    l :: acc

                Err _ ->
                    acc
    in
    List.foldr decodeItem [] raw


decodeNested : Decode.Decoder LogLine
decodeNested =
    doubleEncoded decode
