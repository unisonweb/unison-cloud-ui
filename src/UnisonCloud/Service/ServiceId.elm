module UnisonCloud.Service.ServiceId exposing (..)

import Json.Decode as Decode
import Lib.Decode.Helpers as DecodeH
import Regex


type ServiceId
    = ServiceId String


fromString : String -> Maybe ServiceId
fromString s =
    let
        validate s_ =
            if isValidId s_ then
                Just s_

            else
                Nothing
    in
    s
        |> validate
        |> Maybe.map ServiceId


toString : ServiceId -> String
toString (ServiceId id_) =
    id_


isValidId : String -> Bool
isValidId raw =
    let
        re =
            Maybe.withDefault Regex.never <|
                Regex.fromString "^[a-zA-Z0-9-]*$"
    in
    Regex.contains re raw


equals : ServiceId -> ServiceId -> Bool
equals (ServiceId a) (ServiceId b) =
    a == b



-- DECODE


decode : Decode.Decoder ServiceId
decode =
    Decode.map fromString Decode.string
        |> Decode.andThen (DecodeH.failInvalid "Invalid ServiceId")
