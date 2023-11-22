module UnisonCloud.ServiceHash exposing (..)

import Html exposing (Html, span, text)
import Html.Attributes exposing (class)
import Json.Decode as Decode
import Lib.Util as Util
import Regex
import UI.CopyOnClick as CopyOnClick
import UI.Icon as Icon
import UI.Tooltip as Tooltip


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
                Regex.fromString "^[a-zA-Z0-9-_]*$"
    in
    Regex.contains re raw


toString : ServiceHash -> String
toString serviceHash_ =
    toString_ "#" serviceHash_


toUnprefixedString : ServiceHash -> String
toUnprefixedString serviceHash_ =
    toString_ "" serviceHash_


toString_ : String -> ServiceHash -> String
toString_ prefix_ (ServiceHash h) =
    prefix_ ++ h


{-| Converts a Hash to a shortened (9 characters including the `#` character)
of the raw hash value.

Example:

  - ServiceHash "cv93ajol371idlcd47do5g3nmj7...4s829ofv57mi19pls3l630" -> "#cv93ajol"

-}
toShortString : ServiceHash -> String
toShortString h =
    toShortString_ "#" h


toUnprefixedShortString : ServiceHash -> String
toUnprefixedShortString h =
    toShortString_ "" h


toShortString_ : String -> ServiceHash -> String
toShortString_ p h =
    let
        shorten =
            String.left 9
    in
    h |> toString_ p |> shorten


toUrlString : ServiceHash -> String
toUrlString (ServiceHash h) =
    h


toApiString : ServiceHash -> String
toApiString (ServiceHash h) =
    h


equals : ServiceHash -> ServiceHash -> Bool
equals (ServiceHash a) (ServiceHash b) =
    a == b



-- VIEW


view : (String -> msg) -> ServiceHash -> Html msg
view onCopyMsg hash =
    let
        view_ =
            Tooltip.rich
                (span [ class "service-hash_tooltip" ]
                    [ Icon.view Icon.clipboard, text "Click to copy the full hash" ]
                )
                |> Tooltip.tooltip
                |> Tooltip.view
                    (span [ class "service-hash" ]
                        [ Icon.view Icon.hash, text (toUnprefixedShortString hash) ]
                    )
    in
    CopyOnClick.view onCopyMsg (toString hash) view_



-- DECODE


decode : Decode.Decoder ServiceHash
decode =
    Decode.map fromApiString Decode.string
        |> Decode.andThen (Util.decodeFailInvalid "Invalid ServiceHash")
