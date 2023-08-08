module UnisonCloud.Route exposing
    ( Route(..)
    , fromUrl
    , navigate
    , overview
    , service
    , services
    , toRoute
    , toUrlPattern
    , toUrlString
    )

import Browser.Navigation as Nav
import Code.Definition.Reference exposing (Reference(..))
import Code.HashQualified exposing (HashQualified(..))
import Code.UrlParsers exposing (b, s, slash)
import Parser exposing ((|.), (|=), Parser, end, oneOf, succeed)
import UnisonCloud.AppError as AppError exposing (AppError)
import UnisonCloud.ServiceHash as ServiceHash exposing (ServiceHash)
import Url exposing (Url)
import Url.Builder exposing (relative, string)


type Route
    = Overview
    | Services
    | Service ServiceHash
    | Error AppError
    | NotFound String



-- CREATE ---------------------------------------------------------------------


{-| Overview would normally be the home page, but since we only have Services
right now, the service list page is the home page.
-}
overview : Route
overview =
    Overview


services : Route
services =
    Services


service : ServiceHash -> Route
service sh =
    Service sh



-- PARSE ----------------------------------------------------------------------


toRoute : Maybe String -> Parser Route
toRoute queryString =
    oneOf
        [ -- b overviewParser,
          b servicesParser
        , b serviceParser
        , b (errorParser queryString)
        ]



{-
   overviewParser : Parser Route
   overviewParser =
       succeed Overview |. slash |. end
-}


{-| Taking over from Overview as the home page until we can do more stuff in
the cloud than services
-}
servicesParser : Parser Route
servicesParser =
    -- succeed Services |. slash |. s "services" |. end
    succeed Services |. slash |. end


serviceHashParser : Parser ServiceHash
serviceHashParser =
    let
        parseMaybe mhash =
            case mhash of
                Just s_ ->
                    Parser.succeed s_

                Nothing ->
                    Parser.problem "Invalid ServiceHash"
    in
    Parser.chompUntilEndOr "/"
        |> Parser.getChompedString
        |> Parser.map ServiceHash.fromUrlString
        |> Parser.andThen parseMaybe


serviceParser : Parser Route
serviceParser =
    succeed Service |. slash |. s "services" |. slash |= serviceHashParser |. end


errorParser : Maybe String -> Parser Route
errorParser queryString =
    let
        appErrorQueryParamParser : Parser AppError
        appErrorQueryParamParser =
            oneOf
                [ b (succeed AppError.SignInNoCloudAccount |. s "appError=SignInNoCloudAccount")
                , b (succeed AppError.UnspecifiedError |. s "appError=UnspecifiedError")
                , b (succeed AppError.UnspecifiedError)
                ]

        appError : AppError
        appError =
            queryString
                |> Maybe.withDefault ""
                |> Parser.run appErrorQueryParamParser
                |> Result.withDefault AppError.UnspecifiedError
    in
    succeed (Error appError) |. slash |. s "error"


{-| In environments like Unison Local, the UI is served with a base path

This means that a route to a definition might look like:

  - "/:some-token/ui/latest/terms/base/List/map"
    (where "/:some-token/ui/" is the base path.)

The base path is determined outside of the Elm app using the <base> tag in the
<head> section of the document. The Browser uses this tag to prefix all links.

The base path must end in a slash for links to work correctly, but our parser
expects a path to starts with a slash. When parsing the URL we thus pre-process
the path to strip the base path and ensure a slash prefix before we parse.

---

Note that this doesn't use Url.Parser to parse the URL as you'd normally see in
Elm apps. This is because of how we represent Fully Qualified Names in the url
by turning `.` into `/`:

`base.data.List.map` is represented in the url as `base/data/List/map`

-}
fromUrl : String -> Url -> Route
fromUrl basePath url =
    let
        stripBasePath path =
            if basePath == "/" then
                path

            else
                String.replace basePath "" path

        ensureSlashPrefix path =
            if String.startsWith "/" path then
                path

            else
                "/" ++ path

        parse queryString path =
            Result.withDefault (NotFound path) (Parser.run (toRoute queryString) path)
    in
    url
        |> .path
        |> stripBasePath
        |> ensureSlashPrefix
        |> parse url.query



-- HELPERS --------------------------------------------------------------------


{-| Creates the string of a route in a de-parameritized way for deduping pages in metrics events
-}
toUrlPattern : Route -> String
toUrlPattern r =
    case r of
        Overview ->
            "overview"

        Services ->
            "services"

        Service _ ->
            "services/:service-hash"

        Error _ ->
            "error"

        NotFound _ ->
            "404"


toUrlString : Route -> String
toUrlString route =
    let
        ( path, queryParams ) =
            case route of
                Overview ->
                    ( [], [] )

                Services ->
                    ( [ "services" ], [] )

                Service sh ->
                    ( [ "services", ServiceHash.toUrlString sh ], [] )

                Error e ->
                    ( [ "error" ], [ string "appError" (AppError.toString e) ] )

                NotFound _ ->
                    ( [], [] )
    in
    relative path queryParams



-- EFFECTS


navigate : Nav.Key -> Route -> Cmd msg
navigate navKey route =
    route
        |> toUrlString
        |> Nav.pushUrl navKey
