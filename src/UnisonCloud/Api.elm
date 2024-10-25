module UnisonCloud.Api exposing
    ( assignedServiceDeploys
    , createServiceAssignment
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
import Lib.UserHandle as UserHandle exposing (UserHandle)
import UI.DateTime exposing (DateTime)
import UnisonCloud.FetchLogParams as FetchLogParams exposing (FetchLogParams)
import UnisonCloud.Service.ServiceName as ServiceName exposing (ServiceName)
import UnisonCloud.ServiceHash as ServiceHash exposing (ServiceHash, fromUrlString)


session : Endpoint
session =
    GET { path = [ "account" ], queryParams = [] }


services : Endpoint
services =
    GET { path = [ "services" ], queryParams = [] }


createServiceAssignment : UserHandle -> ServiceName -> ServiceHash -> Endpoint
createServiceAssignment handle serviceName serviceHash =
    POST
        { path =
            [ "users"
            , UserHandle.toUnprefixedString handle
            , "assign"
            , ServiceName.toString serviceName
            , ServiceHash.toUnprefixedString serviceHash
            ]
        , queryParams = []
        , body = Http.emptyBody
        }


service : UserHandle -> ServiceName -> Endpoint
service handle name =
    GET
        { path =
            [ "users"
            , UserHandle.toUnprefixedString handle
            , "services"
            , ServiceName.toString name
            ]
        , queryParams = []
        }


serviceDeploy : ServiceHash -> Endpoint
serviceDeploy sh =
    GET { path = [ "deployments", ServiceHash.toApiString sh ], queryParams = [] }


undeployServiceDeploy : ServiceHash -> Endpoint
undeployServiceDeploy sh =
    DELETE { path = [ "deployments", ServiceHash.toApiString sh ], queryParams = [] }


assignedServiceDeploys : UserHandle -> ServiceName -> Endpoint
assignedServiceDeploys handle serviceName =
    GET
        { path =
            [ "users"
            , UserHandle.toUnprefixedString handle
            , "services"
            , ServiceName.toString serviceName
            , "deployments"
            ]
        , queryParams = []
        }


unassignedServiceDeploys : Endpoint
unassignedServiceDeploys =
    GET { path = [ "deployments", "unassigned" ], queryParams = [] }


serviceLogs : DateTime -> DateTime -> UserHandle -> ServiceName -> FetchLogParams -> Endpoint
serviceLogs fortyEightHoursAgo now handle sName params =
    GET
        { path =
            [ "users"
            , UserHandle.toUnprefixedString handle
            , "logs"
            , "service"
            , ServiceName.toString sName
            ]
        , queryParams = FetchLogParams.toQueryParams fortyEightHoursAgo now params
        }


serviceDeployLogs : DateTime -> DateTime -> ServiceHash -> FetchLogParams -> Endpoint
serviceDeployLogs fortyEightHoursAgo now sh params =
    GET
        { path = [ "logs", "deployment", ServiceHash.toApiString sh ]
        , queryParams = FetchLogParams.toQueryParams fortyEightHoursAgo now params
        }
