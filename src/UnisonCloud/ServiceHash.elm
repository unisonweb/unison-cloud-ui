module UnisonCloud.ServiceHash exposing (..)

import Json.Decode as Decode
import Lib.Util as Util
import Regex


type ServiceHash
    = ServiceHash String


unsafeFromString : String -> ServiceHash
unsafeFromString s =
    ServiceHash s


fromString : String -> Maybe ServiceHash
fromString =
    fromPrefixedString


fromUrlString : String -> Maybe ServiceHash
fromUrlString =
    fromUnprefixedString


fromApiString : String -> Maybe ServiceHash
fromApiString =
    fromUnprefixedString


fromPrefixedString : String -> Maybe ServiceHash
fromPrefixedString raw =
    if String.startsWith "#" raw then
        fromString_ raw

    else
        Nothing


fromUnprefixedString : String -> Maybe ServiceHash
fromUnprefixedString raw =
    if String.startsWith "#" raw then
        Nothing

    else
        fromString_ raw


fromString_ : String -> Maybe ServiceHash
fromString_ raw =
    let
        stripHashSymbol s =
            if String.startsWith "#" s then
                String.dropLeft 1 s

            else
                s

        validate s =
            if isValidHash s then
                Just s

            else
                Nothing
    in
    raw
        |> stripHashSymbol
        |> validate
        |> Maybe.map ServiceHash


isValidHash : String -> Bool
isValidHash raw =
    let
        re =
            Maybe.withDefault Regex.never <|
                Regex.fromString "[a-zA-Z0-9_]"
    in
    Regex.contains re raw


toString : ServiceHash -> String
toString (ServiceHash h) =
    "#" ++ h


toUrlString : ServiceHash -> String
toUrlString (ServiceHash h) =
    h


toApiString : ServiceHash -> String
toApiString (ServiceHash h) =
    h



-- DECODE


decode : Decode.Decoder ServiceHash
decode =
    Decode.map fromApiString Decode.string
        |> Decode.andThen (Util.decodeFailInvalid "Invalid ServiceHash")
