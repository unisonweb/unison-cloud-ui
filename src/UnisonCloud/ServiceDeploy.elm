module UnisonCloud.ServiceDeploy exposing (..)

import Json.Decode as Decode exposing (nullable)
import Json.Decode.Pipeline exposing (required)
import UI.DateTime as DateTime exposing (DateTime)
import UnisonCloud.ServiceHash as ServiceHash exposing (ServiceHash)


type alias ServiceDeploy =
    { hash : ServiceHash
    , deployedAt : DateTime
    , undeployedAt : Maybe DateTime
    }



-- DECODE


decode : Decode.Decoder ServiceDeploy
decode =
    let
        makeServiceDeploy hash deployedAt undeployedAt =
            { hash = hash
            , deployedAt = deployedAt
            , undeployedAt = undeployedAt
            }
    in
    Decode.succeed makeServiceDeploy
        |> required "serviceHash" ServiceHash.decode
        |> required "serviceDeployedAt" DateTime.decode
        |> required "serviceUndeployedAt" (nullable DateTime.decode)
