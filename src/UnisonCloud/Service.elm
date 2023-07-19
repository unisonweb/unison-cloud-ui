module UnisonCloud.Service exposing (..)

import Code.Hash exposing (Hash)
import Url exposing (Url)


type ServiceHash
    = ServiceHash Hash


type ServiceId
    = ServiceId String


type ServiceType
    = Native
    | Web Url


type alias Service =
    { id : ServiceId
    , type_ : ServiceType
    }
