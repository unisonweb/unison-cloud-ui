module UnisonCloud.Service exposing (..)

import Json.Decode as Decode exposing (nullable)
import Json.Decode.Pipeline exposing (required)
import UnisonCloud.ServiceDeploy as ServiceDeploy exposing (ServiceDeploy)


type ServiceId
    = ServiceId String


type alias Service =
    { id : ServiceId
    , name : String
    , latestDeploy : Maybe ServiceDeploy
    }



-- HELPERS


serviceIdToString : ServiceId -> String
serviceIdToString (ServiceId id_) =
    id_



-- DECODE


decode : Decode.Decoder Service
decode =
    let
        makeService rawId name latestDeploy =
            { id = ServiceId rawId
            , name = name
            , latestDeploy = latestDeploy
            }
    in
    Decode.succeed makeService
        |> required "serviceId" Decode.string
        |> required "serviceName" Decode.string
        |> required "latestDeploy" (nullable ServiceDeploy.decode)
