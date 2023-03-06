module UnisonCloud.Account exposing (..)

import Json.Decode as Decode exposing (field, maybe, string)
import Lib.UserHandle as UserHandle exposing (UserHandle)
import Lib.Util exposing (decodeUrl)
import UI.Avatar as Avatar exposing (Avatar)
import UI.Icon as Icon
import Url exposing (Url)


type alias Account a =
    { a
        | handle : UserHandle
        , name : Maybe String
        , avatarUrl : Maybe Url
    }


type alias AccountSummary =
    Account {}



-- HELPERS


name : Account a -> String
name account =
    Maybe.withDefault (UserHandle.toString account.handle) account.name


toAvatar : Account a -> Avatar msg
toAvatar account =
    Avatar.avatar account.avatarUrl (Just (name account))
        |> Avatar.withIcon Icon.user



-- DECODE


decodeSummary : Decode.Decoder AccountSummary
decodeSummary =
    let
        makeSummary handle name_ avatarUrl =
            { handle = handle
            , name = name_
            , avatarUrl = avatarUrl
            }
    in
    Decode.map3 makeSummary
        (field "handle" UserHandle.decodeUnprefixed)
        (maybe (field "name" string))
        (maybe (field "avatarUrl" decodeUrl))
