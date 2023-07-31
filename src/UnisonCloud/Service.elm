module UnisonCloud.Service exposing (..)

import Json.Decode as Decode exposing (nullable)
import Json.Decode.Pipeline exposing (required)
import UI.DateTime as DateTime exposing (DateTime)
import UnisonCloud.ServiceHash as ServiceHash exposing (ServiceHash)
import Url exposing (Url)


type ServiceId
    = ServiceId String


type ServiceType
    = Native
    | Web Url


type alias Service =
    { id : ServiceId
    , hash : ServiceHash
    , type_ : ServiceType
    , deployedAt : DateTime
    , undeployedAt : Maybe DateTime
    }



-- DECODE


decode : Decode.Decoder Service
decode =
    let
        makeService rawId hash deployedAt undeployedAt =
            { id = ServiceId rawId
            , hash = hash
            , type_ = Native
            , deployedAt = deployedAt
            , undeployedAt = undeployedAt
            }
    in
    Decode.succeed makeService
        |> required "serviceId" Decode.string
        |> required "serviceHash" ServiceHash.decode
        |> required "serviceDeployedAt" DateTime.decode
        |> required "serviceUndeployedAt" (nullable DateTime.decode)
