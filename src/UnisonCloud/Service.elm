module UnisonCloud.Service exposing (..)

import Json.Decode as Decode exposing (string)
import Json.Decode.Pipeline exposing (optional, required)
import Set exposing (Set)
import UnisonCloud.Service.ServiceId as ServiceId exposing (ServiceId)
import UnisonCloud.Service.ServiceName as ServiceName exposing (ServiceName)
import UnisonCloud.ServiceDeploy as ServiceDeploy exposing (ServiceDeploy)


type alias Service =
    { id : ServiceId
    , name : ServiceName
    , latestDeploy : Maybe ServiceDeploy
    , tags : Set String
    }



-- DECODE


decode : Decode.Decoder Service
decode =
    let
        makeService id_ name latestDeploy tags =
            { id = id_
            , name = name
            , latestDeploy = latestDeploy
            , tags = Set.fromList tags
            }
    in
    Decode.succeed makeService
        |> required "id" ServiceId.decode
        |> required "name" ServiceName.decode
        |> optional "latestServiceDeploy" (Decode.map Just ServiceDeploy.decode) Nothing
        |> optional "tags" (Decode.list string) []
