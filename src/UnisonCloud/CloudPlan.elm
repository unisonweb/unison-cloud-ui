module UnisonCloud.CloudPlan exposing (CloudPlan(..), decode, fromString, toString)

import Json.Decode as Decode


type CloudPlan
    = Free
    | Starter
    | Pro


fromString : String -> CloudPlan
fromString s =
    case s of
        "Free" ->
            Free

        "Starter" ->
            Starter

        "Pro" ->
            Pro

        _ ->
            Free


toString : CloudPlan -> String
toString p =
    case p of
        Free ->
            "Free"

        Starter ->
            "Starter"

        Pro ->
            "Pro"


decode : Decode.Decoder CloudPlan
decode =
    Decode.map fromString Decode.string
