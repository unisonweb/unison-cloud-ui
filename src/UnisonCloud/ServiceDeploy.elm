module UnisonCloud.ServiceDeploy exposing (..)

import Json.Decode as Decode exposing (string)
import Json.Decode.Pipeline exposing (optional, required)
import Set exposing (Set)
import UI.DateTime as DateTime exposing (DateTime)
import UnisonCloud.ServiceHash as ServiceHash exposing (ServiceHash)
import UnisonCloud.User as User exposing (UserSummary)


type alias ServiceDeploy sd =
    { sd
        | hash : ServiceHash
        , deployedBy : UserSummary
        , deployedAt : DateTime
        , exposedAt : Maybe DateTime
        , tags : Set String
    }


type alias ServiceDeploySummary =
    ServiceDeploy {}


isExposed : ServiceDeploy sd -> Bool
isExposed d =
    d.exposedAt /= Nothing



-- DECODE


decodeAssigned : Decode.Decoder ServiceDeploySummary
decodeAssigned =
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
        |> required "deployedBy" User.decodeSummary
        |> required "deployedAt" DateTime.decode
        |> optional "exposedAt" (Decode.map Just DateTime.decode) Nothing
        |> optional "tags" (Decode.list string) []


decodeSummary : Decode.Decoder ServiceDeploySummary
decodeSummary =
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
        |> required "deployedBy" User.decodeSummary
        |> required "deployedAt" DateTime.decode
        |> optional "exposedAt" (Decode.map Just DateTime.decode) Nothing
        |> optional "tags" (Decode.list string) []
