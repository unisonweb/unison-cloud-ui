module UnisonCloud.ServiceType exposing (..)

import Url exposing (Url)


type ServiceType
    = Native
    | Web Url
