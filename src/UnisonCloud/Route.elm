{-
   Note that this doesn't use Url.Parser to parse the URL as you'd normally see in
   Elm apps. This is because of how we represent Fully Qualified Names in the url
   by turning `.` into `/`:
   `base.data.List.map` is represented in the url as `base/data/List/map`
-}


module UnisonCloud.Route exposing
    ( Route(..)
    , ServiceRoute(..)
    , fromUrl
    , navigate
    , overview
    , service
    , serviceActivity
    , serviceDeploy
    , serviceDeploysForService
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
import UnisonCloud.Service.ServiceId as ServiceId exposing (ServiceId)
import UnisonCloud.ServiceHash as ServiceHash exposing (ServiceHash)
import Url exposing (Url)
import Url.Builder exposing (relative, string)


type Route
    = Overview
    | Services
    | Service ServiceId ServiceRoute
    | ServiceDeploy ServiceHash
    | Error AppError
    | NotFound String


type ServiceRoute
    = Activity
    | Deploys



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


service : ServiceId -> Route
service =
    serviceActivity


serviceActivity : ServiceId -> Route
serviceActivity name =
    Service name Activity


serviceDeploysForService : ServiceId -> Route
serviceDeploysForService name =
    Service name Deploys


serviceDeploy : ServiceHash -> Route
serviceDeploy sh =
    ServiceDeploy sh



-- PARSE ----------------------------------------------------------------------


toRoute : Maybe String -> Parser Route
toRoute queryString =
    oneOf
        [ b overviewParser
        , b servicesParser
        , b serviceParser
        , b serviceDeployParser
        , b (errorParser queryString)
        ]


overviewParser : Parser Route
overviewParser =
    -- Taking over from Overview as the home page until we can do more stuff in
    -- the cloud than services
    succeed Services |. slash |. end


servicesParser : Parser Route
servicesParser =
    succeed Services |. slash |. s "services" |. end


serviceIdParser : Parser ServiceId
serviceIdParser =
    let
        parseMaybe mid =
            case mid of
                Just s_ ->
                    Parser.succeed s_

                Nothing ->
                    Parser.problem "Invalid ServiceId"
    in
    Parser.chompUntilEndOr "/"
        |> Parser.getChompedString
        |> Parser.map ServiceId.fromString
        |> Parser.andThen parseMaybe


serviceParser : Parser Route
serviceParser =
    oneOf
        [ b (succeed (\n -> Service n Activity) |. slash |. s "services" |. slash |= serviceIdParser |. end)
        , b (succeed (\n -> Service n Deploys) |. slash |. s "services" |. slash |= serviceIdParser |. slash |. s "deploys" |. end)
        ]


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


serviceDeployParser : Parser Route
serviceDeployParser =
    succeed ServiceDeploy |. slash |. s "service-deploys" |. slash |= serviceHashParser |. end


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


{-| The base path is determined outside of the Elm app using the <base> tag in the
<head> section of the document. The Browser uses this tag to prefix all links.

The base path must end in a slash for links to work correctly, but our parser
expects a path to starts with a slash. When parsing the URL we thus pre-process
the path to strip the base path and ensure a slash prefix before we parse.

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
            path
                |> Parser.run (toRoute queryString)
                |> Result.withDefault (NotFound path)
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

        Service _ Activity ->
            "services/:service-id"

        Service _ Deploys ->
            "services/:service-id/deploys"

        ServiceDeploy _ ->
            "service-deploys/:service-hash"

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

                Service name Activity ->
                    ( [ "services", ServiceId.toString name ], [] )

                Service name Deploys ->
                    ( [ "services", ServiceId.toString name, "deploys" ], [] )

                ServiceDeploy sh ->
                    ( [ "service-deploys", ServiceHash.toUrlString sh ], [] )

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
