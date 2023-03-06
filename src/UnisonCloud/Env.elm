module UnisonCloud.Env exposing (..)

import Browser.Navigation as Nav
import Lib.HttpApi as HttpApi exposing (HttpApi)
import Lib.OperatingSystem as OS exposing (OperatingSystem)


type alias Env =
    { operatingSystem : OperatingSystem
    , basePath : String
    , api : HttpApi
    , websiteApi : HttpApi
    , navKey : Nav.Key
    }


type alias Flags =
    { operatingSystem : String
    , basePath : String
    , apiUrl : String
    , websiteUrl : String
    , xsrfToken : Maybe String
    , appEnv : String
    }


init : Flags -> Nav.Key -> Env
init flags navKey =
    let
        api =
            HttpApi.httpApi True flags.apiUrl flags.xsrfToken
    in
    { operatingSystem = OS.fromString flags.operatingSystem
    , basePath = flags.basePath
    , api = api
    , websiteApi = HttpApi.httpApi False flags.websiteUrl Nothing
    , navKey = navKey
    }
