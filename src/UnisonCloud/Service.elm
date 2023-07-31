module UnisonCloud.Service exposing (..)

import Json.Decode as Decode
import Json.Decode.Pipeline exposing (required)
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
    }



-- DECODE


decode : Decode.Decoder Service
decode =
    let
        makeService rawId hash =
            { id = ServiceId rawId
            , hash = hash
            , type_ = Native
            }
    in
    Decode.succeed makeService
        |> required "serviceId" Decode.string
        |> required "serviceHash" ServiceHash.decode
