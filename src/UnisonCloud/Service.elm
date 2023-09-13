module UnisonCloud.Service exposing (..)

import Json.Decode as Decode exposing (string)
import Json.Decode.Pipeline exposing (optional, required)
import Lib.UserHandle as UserHandle
import Set exposing (Set)
import UnisonCloud.AppContext exposing (AppContext)
import UnisonCloud.Service.ServiceId as ServiceId exposing (ServiceId)
import UnisonCloud.Service.ServiceName as ServiceName exposing (ServiceName)
import UnisonCloud.ServiceDeploy as ServiceDeploy exposing (ServiceDeploy, ServiceDeploySummary)
import UnisonCloud.ServiceHash as ServiceHash
import Url exposing (Url)


type alias Service =
    { id : ServiceId
    , name : ServiceName
    , activeDeploy : Maybe ServiceDeploySummary
    , tags : Set String
    }


isExposed : Service -> Bool
isExposed serv =
    serv.activeDeploy
        |> Maybe.map ServiceDeploy.isExposed
        |> Maybe.withDefault False


exposedUrl : AppContext -> Service -> Maybe Url
exposedUrl appContext serv =
    let
        makeUrl d =
            Url.fromString
                ("https://"
                    ++ UserHandle.toUnprefixedString d.deployedBy.handle
                    ++ "."
                    ++ appContext.exposedServiceDomain
                    ++ "/s/"
                    ++ ServiceName.toString serv.name
                    ++ "/"
                )

        withDeploy d =
            if ServiceDeploy.isExposed d then
                makeUrl d

            else
                Nothing
    in
    serv.activeDeploy
        |> Maybe.andThen withDeploy


isActiveDeploy : Service -> ServiceDeploy a -> Bool
isActiveDeploy serv depl =
    serv.activeDeploy
        |> Maybe.map .hash
        |> Maybe.map (ServiceHash.equals depl.hash)
        |> Maybe.withDefault False



-- DECODE


decode : Decode.Decoder Service
decode =
    let
        makeService id_ name activeDeploy tags =
            { id = id_
            , name = name
            , activeDeploy = activeDeploy
            , tags = Set.fromList tags
            }
    in
    Decode.succeed makeService
        |> required "id" ServiceId.decode
        |> required "name" ServiceName.decode
        |> optional "latestServiceDeploy" (Decode.map Just ServiceDeploy.decodeSummary) Nothing
        |> optional "tags" (Decode.list string) []
