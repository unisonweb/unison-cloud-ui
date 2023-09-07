module UnisonCloud.Service.ServiceName exposing (..)

import Json.Decode as Decode
import Lib.Util as Util
import Regex


type ServiceName
    = ServiceName String


fromString : String -> Maybe ServiceName
fromString s =
    let
        validate s_ =
            if isValidName s_ then
                Just s_

            else
                Nothing
    in
    s
        |> validate
        |> Maybe.map ServiceName


toString : ServiceName -> String
toString (ServiceName id_) =
    id_


isValidName : String -> Bool
isValidName raw =
    let
        re =
            Maybe.withDefault Regex.never <|
                Regex.fromString "^[a-zA-Z0-9-]*$"
    in
    Regex.contains re raw


equals : ServiceName -> ServiceName -> Bool
equals (ServiceName a) (ServiceName b) =
    a == b



-- DECODE


decode : Decode.Decoder ServiceName
decode =
    Decode.map fromString Decode.string
        |> Decode.andThen (Util.decodeFailInvalid "Invalid ServiceName")
