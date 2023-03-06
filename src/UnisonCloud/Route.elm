module UnisonCloud.Route exposing
    ( Route(..)
    , fromUrl
    , navigate
    , overview
    , toRoute
    , toUrlPattern
    , toUrlString
    )

import Browser.Navigation as Nav
import Code.Definition.Reference exposing (Reference(..))
import Code.HashQualified exposing (HashQualified(..))
import Code.UrlParsers exposing (b, slash)
import Parser exposing ((|.), (|=), Parser, end, oneOf, succeed)
import Url exposing (Url)
import Url.Builder exposing (relative)


type Route
    = Overview
    | NotFound String



-- CREATE ---------------------------------------------------------------------


overview : Route
overview =
    Overview



-- PARSE ----------------------------------------------------------------------


toRoute : Maybe String -> Parser Route
toRoute _ =
    oneOf
        [ b overviewParser
        ]


overviewParser : Parser Route
overviewParser =
    succeed Overview |. slash |. end


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

        NotFound _ ->
            "404"


toUrlString : Route -> String
toUrlString route =
    let
        ( path, queryParams ) =
            case route of
                Overview ->
                    ( [], [] )

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
