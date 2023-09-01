module UnisonCloud.Api exposing
    ( assignedServiceDeploys
    , createService
    , createServiceAssignment
    , deleteServiceAssignment
    , service
    , serviceDeploy
    , serviceDeployLogs
    , serviceLogs
    , services
    , session
    , unassignedServiceDeploys
    , undeployServiceDeploy
    )

import Http
import Lib.HttpApi exposing (Endpoint(..))
import UnisonCloud.FetchLogParams as FetchLogParams exposing (FetchLogParams)
import UnisonCloud.Service.ServiceId as ServiceId exposing (ServiceId)
import UnisonCloud.Service.ServiceName as ServiceName exposing (ServiceName)
import UnisonCloud.ServiceHash as ServiceHash exposing (ServiceHash)


session : Endpoint
session =
    GET { path = [ "account" ], queryParams = [] }


services : Endpoint
services =
    GET { path = [ "services" ], queryParams = [] }


service : ServiceId -> Endpoint
service serviceId =
    GET { path = [ "services", ServiceId.toString serviceId ], queryParams = [] }


createService : ServiceName -> Endpoint
createService serviceName =
    POST
        { path = [ "services", ServiceName.toString serviceName ]
        , queryParams = []
        , body = Http.emptyBody
        }


createServiceAssignment : ServiceId -> ServiceHash -> Endpoint
createServiceAssignment serviceId serviceHash =
    POST
        { path = [ "services", ServiceId.toString serviceId, "assign", ServiceHash.toUnprefixedString serviceHash ]
        , queryParams = []
        , body = Http.emptyBody
        }


deleteServiceAssignment : ServiceId -> Endpoint
deleteServiceAssignment serviceId =
    POST
        { path = [ "services", ServiceId.toString serviceId, "unassign" ]
        , queryParams = []
        , body = Http.emptyBody
        }


serviceDeploy : ServiceHash -> Endpoint
serviceDeploy sh =
    GET { path = [ "deployments", ServiceHash.toApiString sh ], queryParams = [] }


undeployServiceDeploy : ServiceHash -> Endpoint
undeployServiceDeploy sh =
    DELETE { path = [ "deployments", ServiceHash.toApiString sh ], queryParams = [] }


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
            , "service"
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
