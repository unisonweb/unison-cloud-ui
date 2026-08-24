module UnisonCloud.User exposing
    ( User
    , UserSummary
    , decodeSummary
    , name
    , toAvatar
    )

import Json.Decode as Decode exposing (nullable, string)
import Json.Decode.Pipeline exposing (required)
import Lib.Decode.Helpers exposing (url)
import Lib.UserHandle as UserHandle exposing (UserHandle)
import UI.Avatar as Avatar exposing (Avatar)
import UI.Icon as Icon
import Url exposing (Url)


type alias User u =
    { u
        | handle : UserHandle
        , name : Maybe String
        , avatarUrl : Maybe Url
    }


type alias UserSummary =
    User {}



-- HELPERS


name : User u -> String
name user =
    Maybe.withDefault (UserHandle.toString user.handle) user.name


toAvatar : User u -> Avatar msg
toAvatar user =
    Avatar.avatar user.avatarUrl (Just (name user))
        |> Avatar.withIcon Icon.user



-- DECODE


decodeSummary : Decode.Decoder UserSummary
decodeSummary =
    let
        makeSummary handle name_ avatarUrl =
            { handle = handle
            , name = name_
            , avatarUrl = avatarUrl
            }
    in
    Decode.succeed makeSummary
        |> required "handle" UserHandle.decodeUnprefixed
        |> required "name" (nullable string)
        |> required "avatarUrl" (nullable url)
