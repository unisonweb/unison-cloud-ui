module UnisonCloud.ServiceHash exposing (..)

import Code.Hash as Hash exposing (Hash)


type ServiceHash
    = ServiceHash Hash


fromString : String -> Maybe ServiceHash
fromString =
    Hash.fromString >> Maybe.map ServiceHash


fromUrlString : String -> Maybe ServiceHash
fromUrlString =
    Hash.fromUrlString >> Maybe.map ServiceHash


toString : ServiceHash -> String
toString (ServiceHash h) =
    Hash.toString h


toUrlString : ServiceHash -> String
toUrlString (ServiceHash h) =
    Hash.toUrlString h
