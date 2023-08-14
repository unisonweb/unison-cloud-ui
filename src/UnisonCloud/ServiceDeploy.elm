module UnisonCloud.ServiceDeploy exposing (..)

import Json.Decode as Decode
import Json.Decode.Pipeline exposing (required, requiredAt)
import Lib.UserHandle as UserHandle exposing (UserHandle)
import UI.DateTime as DateTime exposing (DateTime)
import UnisonCloud.ServiceHash as ServiceHash exposing (ServiceHash)


type alias ServiceDeploy =
    { hash : ServiceHash

    -- TODO: deployedBy should be a full user
    , deployedBy : UserHandle
    , deployedAt : DateTime
    }



-- DECODE


decode : Decode.Decoder ServiceDeploy
decode =
    let
        makeServiceDeploy hash deployedBy deployedAt =
            { hash = hash
            , deployedBy = deployedBy
            , deployedAt = deployedAt
            }
    in
    Decode.succeed makeServiceDeploy
        |> required "hash" ServiceHash.decode
        |> requiredAt [ "deployedBy", "handle" ] UserHandle.decodeUnprefixed
        |> required "deployedAt" DateTime.decode
