module UnisonCloud.Service exposing (..)

import Json.Decode as Decode exposing (string)
import Json.Decode.Pipeline exposing (optional, required)
import Lib.Util as Util
import Set exposing (Set)
import UnisonCloud.ServiceDeploy as ServiceDeploy exposing (ServiceDeploy)


type ServiceId
    = ServiceId String


type alias Service =
    { id : ServiceId
    , name : String
    , latestDeploy : Maybe ServiceDeploy
    , tags : Set String
    }



-- HELPERS


serviceIdToString : ServiceId -> String
serviceIdToString (ServiceId id_) =
    id_


{-| TODO: Validate somehow
-}
serviceIdFromString : String -> Maybe ServiceId
serviceIdFromString s =
    Just (ServiceId s)



-- DECODE


decodeServiceId : Decode.Decoder ServiceId
decodeServiceId =
    Decode.map serviceIdFromString Decode.string
        |> Decode.andThen (Util.decodeFailInvalid "Invalid ServiceId")


decode : Decode.Decoder Service
decode =
    let
        makeService id_ name latestDeploy tags =
            { id = id_
            , name = name
            , latestDeploy = latestDeploy
            , tags = Set.fromList tags
            }
    in
    Decode.succeed makeService
        |> required "id" decodeServiceId
        |> required "name" Decode.string
        |> optional "latestServiceDeploy" (Decode.map Just ServiceDeploy.decode) Nothing
        |> optional "tags" (Decode.list string) []
