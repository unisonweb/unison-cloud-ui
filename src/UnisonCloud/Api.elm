module UnisonCloud.Api exposing (service, serviceDeploy, serviceDeployLogs, serviceDeploys, services, session)

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
service id_ =
    GET { path = [ "services", Service.serviceIdToString id_ ], queryParams = [] }


serviceDeploy : ServiceHash -> Endpoint
serviceDeploy sh =
    GET { path = [ "service-deploys", ServiceHash.toString sh ], queryParams = [] }


serviceDeploys : Maybe ServiceId -> Endpoint
serviceDeploys id_ =
    let
        queryParams =
            case id_ of
                Just i ->
                    [ string "serviceId" (Service.serviceIdToString i) ]

                Nothing ->
                    []
    in
    GET { path = [ "service-deploys" ], queryParams = queryParams }


serviceDeployLogs : ServiceHash -> Endpoint
serviceDeployLogs sh =
    GET { path = [ "logs", ServiceHash.toString sh ], queryParams = [] }
