module UnisonCloud.Service exposing (..)

import Json.Decode as Decode
import Json.Decode.Pipeline exposing (optional, required)
import UnisonCloud.ServiceDeploy as ServiceDeploy exposing (ServiceDeploy)


type ServiceId
    = ServiceId String


type alias Service =
    { id : ServiceId
    , name : String
    , latestDeploy : Maybe ServiceDeploy
    }



-- HELPERS


serviceIdToString : ServiceId -> String
serviceIdToString (ServiceId id_) =
    id_



-- DECODE


decode : Decode.Decoder Service
decode =
    let
        makeService rawId name latestDeploy =
            { id = ServiceId rawId
            , name = name
            , latestDeploy = latestDeploy
            }
    in
    Decode.succeed makeService
        |> required "id" Decode.string
        |> required "name" Decode.string
        |> optional "latestServiceDeploy" (Decode.map Just ServiceDeploy.decode) Nothing



{-

   serviceHash : "BqVhDrNgHddFrNsEDRuxTUkeJUrnAGY8bFTNpe_r24Q"
   serviceHistory :
   [{serviceAssignmentHash: "BqVhDrNgHddFrNsEDRuxTUkeJUrnAGY8bFTNpe_r24Q",…}]
   serviceUserId : "U-141c4ddf-2423-4f10-a4de-465939951354"

-}
