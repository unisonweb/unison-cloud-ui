module UnisonCloud.AppError exposing (..)


type AppError
    = UnspecifiedError
    | SignInNoCloudAccount


fromString : String -> AppError
fromString s =
    case s of
        "SignInNoCloudAccount" ->
            SignInNoCloudAccount

        "UnspecifiedError" ->
            UnspecifiedError

        _ ->
            UnspecifiedError


toString : AppError -> String
toString e =
    case e of
        SignInNoCloudAccount ->
            "SignInNoCloudAccount"

        UnspecifiedError ->
            "UnspecifiedError"
