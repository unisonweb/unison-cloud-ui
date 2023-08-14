module UnisonCloud.Service exposing (..)

import Json.Decode as Decode exposing (string)
import Json.Decode.Pipeline exposing (optional, required)
import Set exposing (Set)
import UnisonCloud.ServiceDeploy as ServiceDeploy exposing (ServiceDeploy)


type ServiceId
    = ServiceId String


type alias Service =
    { id : ServiceId
    , name : String
    , latestDeploy : Maybe ServiceDeploy
    , tags : Set String
    }



-- HELPERS


serviceIdToString : ServiceId -> String
serviceIdToString (ServiceId id_) =
    id_



-- DECODE


decode : Decode.Decoder Service
decode =
    let
        makeService rawId name latestDeploy tags =
            { id = ServiceId rawId
            , name = name
            , latestDeploy = latestDeploy
            , tags = Set.fromList tags
            }
    in
    Decode.succeed makeService
        |> required "id" Decode.string
        |> required "name" Decode.string
        |> optional "latestServiceDeploy" (Decode.map Just ServiceDeploy.decode) Nothing
        |> optional "tags" (Decode.list string) []
