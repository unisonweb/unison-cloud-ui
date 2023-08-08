module UnisonCloud.Service exposing (..)

import Json.Decode as Decode exposing (nullable)
import Json.Decode.Pipeline exposing (required)
import UnisonCloud.ServiceDeploy as ServiceDeploy exposing (ServiceDeploy)


type ServiceId
    = ServiceId String


type alias Service =
    { id : ServiceId
    , name : Maybe String
    , latestDeploy : Maybe ServiceDeploy
    }



-- HELPERS


label : Service -> String
label { id, name } =
    let
        (ServiceId id_) =
            id
    in
    Maybe.withDefault id_ name



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
        |> required "serviceName" (nullable Decode.string)
        |> required "latestDeploy" (nullable ServiceDeploy.decode)
