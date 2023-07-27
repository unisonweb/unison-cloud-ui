module UnisonCloud.ScrollEvent exposing (..)

import Json.Decode as Decode exposing (int)
import Json.Decode.Pipeline exposing (requiredAt)


type alias ScrollEvent =
    { scrollHeight : Int
    , scrollTop : Int
    , clientHeight : Int
    }


decode : Decode.Decoder ScrollEvent
decode =
    Decode.succeed ScrollEvent
        |> requiredAt [ "currentTarget", "scrollHeight" ] int
        |> requiredAt [ "currentTarget", "scrollTop" ] int
        |> requiredAt [ "currentTarget", "clientHeight" ] int


decodeToMsg : (ScrollEvent -> msg) -> Decode.Decoder msg
decodeToMsg toMsg =
    Decode.map toMsg decode
