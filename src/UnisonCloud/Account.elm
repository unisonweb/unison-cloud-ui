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
        , organizationMemberships : List OrganizationMembership
    }


type OrganizationMembership
    = OrganizationMembership UserHandle


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


isOrganizationMember : UserHandle -> Account a -> Bool
isOrganizationMember orgHandle account =
    account.organizationMemberships
        |> List.map (\(OrganizationMembership handle) -> handle)
        |> List.member orgHandle



-- DECODE


decodeSummary : Decode.Decoder AccountSummary
decodeSummary =
    let
        makeSummary handle name_ avatarUrl orgs =
            { handle = handle
            , name = name_
            , avatarUrl = avatarUrl
            , organizationMemberships = orgs
            }
    in
    Decode.map4 makeSummary
        (field "handle" UserHandle.decodeUnprefixed)
        (maybe (field "name" string))
        (maybe (field "avatarUrl" decodeUrl))
        (field "organizationMemberships" (Decode.list (Decode.map OrganizationMembership UserHandle.decodeUnprefixed)))
