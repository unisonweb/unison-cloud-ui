module UnisonCloud.AppContext exposing (..)

import Browser.Navigation as Nav
import Lib.HttpApi as HttpApi exposing (HttpApi)
import Lib.OperatingSystem as OS exposing (OperatingSystem)
import Time
import UI.DateTime exposing (DateTime)
import UnisonCloud.Session exposing (Session)
import Url exposing (Url)


type alias AppContext =
    { session : Session
    , operatingSystem : OperatingSystem
    , basePath : String
    , api : HttpApi
    , currentUrl : Url
    , websiteApi : HttpApi
    , exposedServiceDomain : String
    , navKey : Nav.Key
    , now : DateTime
    , timeZone : Time.Zone
    }


type alias Flags =
    { operatingSystem : String
    , basePath : String
    , apiUrl : String
    , websiteUrl : String
    , xsrfToken : Maybe String
    , appEnv : String
    }


init : Flags -> Nav.Key -> Url -> DateTime -> Time.Zone -> Session -> AppContext
init flags navKey currentUrl now timeZone session =
    let
        api =
            HttpApi.httpApi True flags.apiUrl flags.xsrfToken
    in
    { session = session
    , operatingSystem = OS.fromString flags.operatingSystem
    , basePath = flags.basePath
    , currentUrl = currentUrl
    , api = api
    , websiteApi = HttpApi.httpApi False flags.websiteUrl Nothing
    , exposedServiceDomain = "unison-services.cloud"
    , navKey = navKey
    , now = now
    , timeZone = timeZone
    }
