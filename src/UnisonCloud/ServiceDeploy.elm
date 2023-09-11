module UnisonCloud.ServiceDeploy exposing (..)

import Json.Decode as Decode exposing (string)
import Json.Decode.Pipeline exposing (optional, required)
import Set exposing (Set)
import UI.DateTime as DateTime exposing (DateTime)
import UnisonCloud.AppContext exposing (AppContext)
import UnisonCloud.Service.ServiceId exposing (ServiceId)
import UnisonCloud.ServiceHash as ServiceHash exposing (ServiceHash)
import UnisonCloud.User as User exposing (UserSummary)
import Url exposing (Url)


type Deployed
    = Deployed { deployedBy : UserSummary, deployedAt : DateTime }
    | Undeployed
        { originallDeployedBt : UserSummary
        , originallyDeployedAt : DateTime
        , undeployedAt : DateTime
        }


type Exposed
    = Exposed { exposedAt : DateTime }
    | Unexposed
        { originallyExposedAt : DateTime
        , unexposedAt : DateTime
        }


type Assigned
    = Assigned { assignedAt : DateTime, assignedTo : ServiceId }
    | Unassigned
        { originallAssignedAt : DateTime
        , originallyAssignedTo : ServiceId
        , unassignedAt : DateTime
        }


type alias ServiceDeploy sd =
    { sd
        | hash : ServiceHash
        , deployedBy : UserSummary
        , deployedAt : DateTime
        , undeployedAt : Maybe DateTime
        , exposedAt : Maybe DateTime
        , unexposedAt : Maybe DateTime
        , tags : Set String
    }


type alias ServiceDeploySummary =
    ServiceDeploy {}


isExposed : ServiceDeploy sd -> Bool
isExposed d =
    d.exposedAt /= Nothing


isLive : ServiceDeploy sd -> Bool
isLive =
    isUndeployed >> not


isUndeployed : ServiceDeploy sd -> Bool
isUndeployed d =
    d.undeployedAt /= Nothing


exposedUrl : AppContext -> ServiceDeploy sd -> Maybe Url
exposedUrl appContext d =
    if isExposed d then
        Url.fromString
            ("https://"
                ++ appContext.exposedServiceDomain
                ++ "/h/"
                ++ ServiceHash.toUnprefixedString d.hash
                ++ "/"
            )

    else
        Nothing



-- DECODE


decodeAssigned : Decode.Decoder ServiceDeploySummary
decodeAssigned =
    let
        makeServiceDeploy hash deployedBy deployedAt undeployedAt exposedAt unexposedAt tags =
            { hash = hash
            , deployedBy = deployedBy
            , deployedAt = deployedAt
            , undeployedAt = undeployedAt
            , exposedAt = exposedAt
            , unexposedAt = unexposedAt
            , tags = Set.fromList tags
            }
    in
    Decode.succeed makeServiceDeploy
        |> required "hash" ServiceHash.decode
        |> required "deployedBy" User.decodeSummary
        |> required "deployedAt" DateTime.decode
        |> optional "undeployedAt" (Decode.map Just DateTime.decode) Nothing
        |> optional "exposedAt" (Decode.map Just DateTime.decode) Nothing
        |> optional "unexposedAt" (Decode.map Just DateTime.decode) Nothing
        |> optional "tags" (Decode.list string) []


decodeSummary : Decode.Decoder ServiceDeploySummary
decodeSummary =
    let
        makeServiceDeploy hash deployedBy deployedAt undeployedAt exposedAt unexposedAt tags =
            { hash = hash
            , deployedBy = deployedBy
            , deployedAt = deployedAt
            , undeployedAt = undeployedAt
            , exposedAt = exposedAt
            , unexposedAt = unexposedAt
            , tags = Set.fromList tags
            }
    in
    Decode.succeed makeServiceDeploy
        |> required "hash" ServiceHash.decode
        |> required "deployedBy" User.decodeSummary
        |> required "deployedAt" DateTime.decode
        |> optional "undeployedAt" (Decode.map Just DateTime.decode) Nothing
        |> optional "exposedAt" (Decode.map Just DateTime.decode) Nothing
        |> optional "unexposedAt" (Decode.map Just DateTime.decode) Nothing
        |> optional "tags" (Decode.list string) []
