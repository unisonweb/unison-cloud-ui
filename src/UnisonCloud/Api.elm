module UnisonCloud.Api exposing
    ( assignedServiceDeploys
    , service
    , serviceDeploy
    , serviceDeployLogs
    , serviceLogs
    , services
    , session
    , unassignedServiceDeploys
    )

import Lib.HttpApi exposing (Endpoint(..))
import UnisonCloud.FetchLogParams as FetchLogParams exposing (FetchLogParams)
import UnisonCloud.Service.ServiceId as ServiceId exposing (ServiceId)
import UnisonCloud.ServiceHash as ServiceHash exposing (ServiceHash)


session : Endpoint
session =
    GET { path = [ "account" ], queryParams = [] }


services : Endpoint
services =
    GET { path = [ "services" ], queryParams = [] }


service : ServiceId -> Endpoint
service sName =
    GET { path = [ "services", ServiceId.toString sName ], queryParams = [] }


serviceDeploy : ServiceHash -> Endpoint
serviceDeploy sh =
    GET { path = [ "deployments", ServiceHash.toApiString sh ], queryParams = [] }


assignedServiceDeploys : ServiceId -> Endpoint
assignedServiceDeploys serviceId =
    GET
        { path = [ "services", ServiceId.toString serviceId, "deployments" ]
        , queryParams = []
        }


unassignedServiceDeploys : Endpoint
unassignedServiceDeploys =
    GET { path = [ "unassigned" ], queryParams = [] }


serviceLogs : ServiceId -> FetchLogParams -> Endpoint
serviceLogs sName params =
    GET
        { path =
            [ "logs"
            , "services"
            , ServiceId.toString sName
            ]
        , queryParams = FetchLogParams.toQueryParams params
        }


serviceDeployLogs : ServiceHash -> FetchLogParams -> Endpoint
serviceDeployLogs sh params =
    GET
        { path = [ "logs", "deployment", ServiceHash.toApiString sh ]
        , queryParams = FetchLogParams.toQueryParams params
        }
