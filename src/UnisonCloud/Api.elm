module UnisonCloud.Api exposing (logs, service, services, session)

import Lib.HttpApi exposing (Endpoint(..))
import UnisonCloud.ServiceHash as ServiceHash exposing (ServiceHash)


session : Endpoint
session =
    GET { path = [ "account" ], queryParams = [] }


services : Endpoint
services =
    GET { path = [ "services" ], queryParams = [] }


service : ServiceHash -> Endpoint
service sh =
    GET { path = [ "services", ServiceHash.toString sh ], queryParams = [] }


logs : ServiceHash -> Endpoint
logs sh =
    GET { path = [ "logs", ServiceHash.toString sh ], queryParams = [] }
