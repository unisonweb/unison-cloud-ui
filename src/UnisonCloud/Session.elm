module UnisonCloud.Session exposing (Session(..), decode, isHandle, isProjectOwner)

import Code.Project.ProjectRef as ProjectRef exposing (ProjectRef)
import Json.Decode as Decode
import Lib.UserHandle as UserHandle exposing (UserHandle)
import UnisonCloud.Account as Account exposing (AccountSummary)


type Session
    = Anonymous
    | SignedIn AccountSummary


isHandle : UserHandle -> Session -> Bool
isHandle handle session =
    case session of
        Anonymous ->
            False

        SignedIn a ->
            UserHandle.equals a.handle handle


isProjectOwner : ProjectRef -> Session -> Bool
isProjectOwner projectRef session =
    isHandle (ProjectRef.handle projectRef) session


decode : Decode.Decoder Session
decode =
    Decode.oneOf
        [ Decode.map SignedIn Account.decodeSummary
        , Decode.succeed Anonymous
        ]
