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
import UnisonCloud.Service.ServiceName as ServiceName exposing (ServiceName)
import UnisonCloud.ServiceHash as ServiceHash exposing (ServiceHash)


session : Endpoint
session =
    GET { path = [ "account" ], queryParams = [] }


services : Endpoint
services =
    GET { path = [ "services" ], queryParams = [] }


service : ServiceName -> Endpoint
service sName =
    GET { path = [ "services", ServiceName.toString sName ], queryParams = [] }


serviceDeploy : ServiceHash -> Endpoint
serviceDeploy sh =
    GET { path = [ "deployments", ServiceHash.toApiString sh ], queryParams = [] }


unassignedServiceDeploys : Endpoint
unassignedServiceDeploys =
    GET { path = [ "unassigned" ], queryParams = [] }


serviceLogs : ServiceName -> FetchLogParams -> Endpoint
serviceLogs sName params =
    GET
        { path =
            [ "logs"
            , "services"
            , ServiceName.toString sName
            ]
        , queryParams = FetchLogParams.toQueryParams params
        }


serviceDeployLogs : ServiceHash -> FetchLogParams -> Endpoint
serviceDeployLogs sh params =
    GET
        { path = [ "logs", "deployment", ServiceHash.toApiString sh ]
        , queryParams = FetchLogParams.toQueryParams params
        }
