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
    GET { path = [ "deployments", ServiceHash.toString sh ], queryParams = [] }


unassignedServiceDeploys : Endpoint
unassignedServiceDeploys =
    GET { path = [ "unassigned" ], queryParams = [] }


serviceLogs : ServiceId -> Endpoint
serviceLogs sid =
    GET { path = [ "logs", "services", Service.serviceIdToString sid ], queryParams = [] }


serviceDeployLogs : ServiceHash -> Endpoint
serviceDeployLogs sh =
    GET
        { path = [ "logs", "deployment", ServiceHash.toApiString sh ]
        , queryParams = []
        }
