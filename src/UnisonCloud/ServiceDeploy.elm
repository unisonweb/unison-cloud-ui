module UnisonCloud.ServiceDeploy exposing (..)

import Json.Decode as Decode exposing (string)
import Json.Decode.Pipeline exposing (optional, required, requiredAt)
import Lib.UserHandle as UserHandle exposing (UserHandle)
import Set exposing (Set)
import UI.DateTime as DateTime exposing (DateTime)
import UI.TabList as TabList
import UnisonCloud.ServiceHash as ServiceHash exposing (ServiceHash)


type alias ServiceDeploy =
    { hash : ServiceHash

    -- TODO: deployedBy should be a full user
    , deployedBy : UserHandle
    , deployedAt : DateTime
    , exposedAt : Maybe DateTime
    , tags : Set String
    }


isExposed : ServiceDeploy -> Bool
isExposed d =
    d.exposedAt /= Nothing



-- DECODE


decode : Decode.Decoder ServiceDeploy
decode =
    let
        makeServiceDeploy hash deployedBy deployedAt exposedAt tags =
            { hash = hash
            , deployedBy = deployedBy
            , deployedAt = deployedAt
            , exposedAt = exposedAt
            , tags = Set.fromList tags
            }
    in
    Decode.succeed makeServiceDeploy
        |> required "hash" ServiceHash.decode
        |> requiredAt [ "deployedBy", "handle" ] UserHandle.decodeUnprefixed
        |> required "deployedAt" DateTime.decode
        |> optional "exposedAt" (Decode.map Just DateTime.decode) Nothing
        |> optional "tags" (Decode.list string) []
