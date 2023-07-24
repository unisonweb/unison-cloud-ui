module UnisonCloud.Api exposing (session)

import Lib.HttpApi exposing (Endpoint(..))


session : Endpoint
session =
    GET { path = [ "account" ], queryParams = [] }
