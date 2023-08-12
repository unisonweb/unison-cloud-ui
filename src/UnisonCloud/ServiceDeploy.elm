module UnisonCloud.ServiceDeploy exposing (..)

import Json.Decode as Decode
import Json.Decode.Pipeline exposing (required)
import UI.DateTime as DateTime exposing (DateTime)
import UnisonCloud.ServiceHash as ServiceHash exposing (ServiceHash)


type alias ServiceDeploy =
    { hash : ServiceHash
    , deployedAt : DateTime
    }



-- DECODE


decode : Decode.Decoder ServiceDeploy
decode =
    let
        makeServiceDeploy hash deployedAt =
            { hash = hash
            , deployedAt = deployedAt
            }
    in
    Decode.succeed makeServiceDeploy
        |> required "hash" ServiceHash.decode
        |> required "deployedAt" DateTime.decode
