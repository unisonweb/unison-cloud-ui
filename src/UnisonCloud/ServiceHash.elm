{- TODO: This is currently wrapping Hash, but thats not really appropriate. It
   should become its own thing. It has different invariants than Hash. For
   instance, it can never be a builtin.
-}


module UnisonCloud.ServiceHash exposing (..)

import Code.Hash as Hash exposing (Hash)
import Json.Decode as Decode


type ServiceHash
    = ServiceHash Hash


unsafeFromString : String -> ServiceHash
unsafeFromString s =
    ServiceHash (Hash.unsafeFromString s)


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


toUnprefixedString : ServiceHash -> String
toUnprefixedString (ServiceHash h) =
    Hash.toUnprefixedString h



-- DECODE


decode : Decode.Decoder ServiceHash
decode =
    Decode.map ServiceHash Hash.decode
