module UnisonCloud.Api exposing
    ( service
    , serviceDeploy
    , serviceDeployLogs
    , serviceDeploys
    , serviceLogs
    , services
    , session
    )

import Lib.HttpApi exposing (Endpoint(..))
import UnisonCloud.Service as Service exposing (ServiceId)
import UnisonCloud.ServiceHash as ServiceHash exposing (ServiceHash)
import Url.Builder exposing (string)


session : Endpoint
session =
    GET { path = [ "account" ], queryParams = [] }


services : Endpoint
services =
    GET { path = [ "services" ], queryParams = [] }


service : ServiceId -> Endpoint
service sid =
    GET { path = [ "services", Service.serviceIdToString sid ], queryParams = [] }


serviceDeploy : ServiceHash -> Endpoint
serviceDeploy sh =
    GET { path = [ "deployments", ServiceHash.toString sh ], queryParams = [] }


serviceDeploys : Maybe ServiceId -> Endpoint
serviceDeploys sid =
    let
        queryParams =
            case sid of
                Just sid_ ->
                    [ string "serviceId" (Service.serviceIdToString sid_) ]

                Nothing ->
                    []
    in
    GET { path = [ "deployments" ], queryParams = queryParams }


serviceLogs : ServiceId -> Endpoint
serviceLogs sid =
    GET { path = [ "logs", "services", Service.serviceIdToString sid ], queryParams = [] }


serviceDeployLogs : ServiceHash -> Endpoint
serviceDeployLogs sh =
    GET { path = [ "logs", "deployments", ServiceHash.toString sh ], queryParams = [] }
