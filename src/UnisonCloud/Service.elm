module UnisonCloud.Service exposing (..)

import Json.Decode as Decode exposing (string)
import Json.Decode.Pipeline exposing (optional, required)
import Lib.UserHandle as UserHandle
import Set exposing (Set)
import UnisonCloud.AppContext exposing (AppContext)
import UnisonCloud.Service.ServiceId as ServiceId exposing (ServiceId)
import UnisonCloud.Service.ServiceName as ServiceName exposing (ServiceName)
import UnisonCloud.ServiceDeploy as ServiceDeploy exposing (ServiceDeploySummary)
import Url exposing (Url)


type alias Service =
    { id : ServiceId
    , name : ServiceName
    , latestDeploy : Maybe ServiceDeploySummary
    , tags : Set String
    }


isExposed : Service -> Bool
isExposed serv =
    serv.latestDeploy
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
                )

        withDeploy d =
            if ServiceDeploy.isExposed d then
                makeUrl d

            else
                Nothing
    in
    serv.latestDeploy
        |> Maybe.andThen withDeploy



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
        |> optional "latestServiceDeploy" (Decode.map Just ServiceDeploy.decodeSummary) Nothing
        |> optional "tags" (Decode.list string) []
