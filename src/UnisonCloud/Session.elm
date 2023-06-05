module UnisonCloud.Session exposing (Session, decode, isHandle)

import Json.Decode as Decode
import Lib.UserHandle as UserHandle exposing (UserHandle)
import UnisonCloud.Account as Account exposing (AccountSummary)


type alias Session =
    AccountSummary


isHandle : UserHandle -> Session -> Bool
isHandle handle session =
    UserHandle.equals session.handle handle


decode : Decode.Decoder Session
decode =
    Account.decodeSummary
