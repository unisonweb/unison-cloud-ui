module UnisonCloud.Api exposing
    ( service
    , serviceDeploy
    , serviceDeployLogs
    , serviceLogs
    , services
    , session
    , unassignedServiceDeploys
    )

import Lib.HttpApi exposing (Endpoint(..))
import UnisonCloud.FetchLogParams as FetchLogParams exposing (FetchLogParams)
import UnisonCloud.Service as Service exposing (ServiceId)
import UnisonCloud.ServiceHash as ServiceHash exposing (ServiceHash)


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
    GET { path = [ "deployments", ServiceHash.toApiString sh ], queryParams = [] }


unassignedServiceDeploys : Endpoint
unassignedServiceDeploys =
    GET { path = [ "unassigned" ], queryParams = [] }


serviceLogs : ServiceId -> FetchLogParams -> Endpoint
serviceLogs sid params =
    GET
        { path =
            [ "logs"
            , "services"
            , Service.serviceIdToString sid
            ]
        , queryParams = FetchLogParams.toQueryParams params
        }


serviceDeployLogs : ServiceHash -> FetchLogParams -> Endpoint
serviceDeployLogs sh params =
    GET
        { path = [ "logs", "deployment", ServiceHash.toApiString sh ]
        , queryParams = FetchLogParams.toQueryParams params
        }
