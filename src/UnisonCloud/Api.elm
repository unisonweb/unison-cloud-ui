module UnisonCloud.Api exposing (session)

import Lib.HttpApi exposing (Endpoint(..))


{-| TODO:
Should likely be a /session endpoint

Instead, for now we're using the /account endpoint, but over time that endpoint
is likely to grow to include data not needed for Session.

-}
session : Endpoint
session =
    GET { path = [ "account" ], queryParams = [] }
