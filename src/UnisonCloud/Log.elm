module UnisonCloud.Log exposing (..)

import Dict
import Html exposing (Html, div, hr, table, tbody, td, text, th, tr)
import Html.Attributes exposing (class, classList)
import Html.Events exposing (on)
import Html.Keyed
import Html.Lazy exposing (lazy)
import Lib.ScrollEvent as ScrollEvent exposing (ScrollEvent)
import Time
import UI
import UI.Button as Button
import UI.DateTime as DateTime exposing (DateTime)
import UI.Icon as Icon
import UI.Sizing as Sizing
import UUID
import UUID.Set as Set exposing (Set)
import UnisonCloud.Env exposing (Env)
import UnisonCloud.LogLevel as LogLevel
import UnisonCloud.LogLine as LogLine exposing (LogLine)



-- 2 directional infinite scroll
-- should start loading well before items are added
-- when items are added the scroll position shouldn't be affected... HOW?
-- https://addyosmani.com/blog/infinite-scroll-without-layout-shifts/
-- MODEL
{-

   Infinite Scroll Challenges
   --------------------------

   * There should not be any yank when scrolling up to look at older log messages
   * We should preload well ahead of the scroll bar, so there's no "loading"
   * We should not jump and yank the UI when new data is rendered
   * We need to scroll and load in both directions (maybe not in v1?)

   * https://addyosmani.com/blog/infinite-scroll-without-layout-shifts/

   One solution could be to pre-render a large empty area outside of the
   viewport, but this requires you to know the total number of lines and the
   height of an line (the latter not being a big deal).

   Instagram doesn't seem to work that way. By looking at the scroll bar when
   scroll you can see that it jumps, but the page itself remains smooth, this
   indicates to me that there isn't a large empty area prerendered, but really
   good scroll position maintenance.

   We could render the new elements off to the side, in an iframe, measure the
   sizes of everything before adding them such that we can calculate the new
   scroll position perfectly and make the switch without the user knowing.

-}


type LogEntry
    = Line LogLine
    | DateBoundary DateTime


type alias LogEntries =
    List LogEntry


type Direction
    = Before
    | After



-- WHERE IS BOOKMARK?


type alias Log =
    { expandedLines : Set
    , freshOldLines : List LogLine
    , logLines : List LogLine
    , freshNewLines : List LogLine
    }


type alias Model =
    { log : Log }


init : Env -> ( Model, Cmd Msg )
init _ =
    ( { log =
            { expandedLines = Set.empty
            , freshOldLines = []
            , logLines = fauxLines
            , freshNewLines = []
            }
      }
    , Cmd.none
    )



-- UPDATE


type Msg
    = LogLinesFetchFinished
    | Scroll ScrollEvent
    | FetchNew
    | FetchOld
    | ToggleLogLine LogLine


update : Env -> Msg -> Model -> ( Model, Cmd Msg )
update _ msg model =
    let
        log =
            model.log
    in
    case msg of
        ToggleLogLine line ->
            let
                log_ =
                    { log | expandedLines = Set.toggle line.id log.expandedLines }
            in
            ( { model | log = log_ }, Cmd.none )

        FetchNew ->
            let
                log_ =
                    { log
                        | logLines = log.freshNewLines ++ log.logLines
                        , freshNewLines = newLines
                    }
            in
            ( { model | log = log_ }, Cmd.none )

        FetchOld ->
            let
                log_ =
                    { log
                        | logLines = log.logLines ++ log.freshOldLines
                        , freshOldLines = oldLines
                    }
            in
            ( { model | log = log_ }, Cmd.none )

        Scroll ev ->
            let
                topOffset =
                    abs (ev.scrollHeight + ev.scrollTop - ev.clientHeight)

                closenessOffset =
                    0

                isCloseToTop =
                    topOffset <= closenessOffset

                log_ =
                    if isCloseToTop then
                        { log | logLines = log.logLines ++ oldLines }

                    else
                        log
            in
            ( { model | log = log_ }, Cmd.none )

        _ ->
            ( model, Cmd.none )



-- HELPERS


logEntryHeight : Sizing.Rem
logEntryHeight =
    Sizing.Rem 1.5



-- EFFECTS


fetchLogLines : Env -> LogLine -> Direction -> Cmd Msg
fetchLogLines _ _ _ =
    Cmd.none



-- VIEW


{-| If there's no message, print out the line data instead of it is present,
finally, if there's no data, render an empty line.

TODO: Add various highlights, like bolding of GET and POST.

-}
viewLogMessage : LogLine -> Html Msg
viewLogMessage line =
    let
        viewRawData =
            if LogLine.hasData line then
                line
                    |> LogLine.dataToList
                    |> List.map (\( k, v ) -> "\"" ++ k ++ "\": " ++ "\"" ++ v)
                    |> String.join ", "
                    |> (\d -> text ("{ " ++ d ++ " }"))
                    |> (\d -> div [ class "log-line_log-message_raw-data" ] [ d ])

            else
                div [ class "log-line_log-message_no-data" ] [ Icon.view Icon.dash ]
    in
    case line.message of
        Nothing ->
            viewRawData

        Just "" ->
            viewRawData

        Just m ->
            div [ class "log-line_log-message_message" ] [ text m ]


viewDataTable : LogLine.LogLineData -> Html Msg
viewDataTable data =
    data
        |> Dict.toList
        |> List.map (\( k, v ) -> tr [] [ th [] [ text k ], td [] [ text v ] ])
        |> (\d -> table [ class "log-line_log-message_data-table" ] [ tbody [] d ])


viewLoggedAt : DateTime -> Html Msg
viewLoggedAt dateTime =
    div [ class "log-line_logged-at" ] [ DateTime.view DateTime.TimeWithSeconds dateTime ]


viewLine : Model -> LogLine -> Html Msg
viewLine model line =
    let
        isExpanded =
            Set.member line.id model.log.expandedLines

        icon =
            if isExpanded then
                Icon.caretDown

            else
                Icon.caretRight

        ( caret, expandable ) =
            if LogLine.hasData line && line.message /= Nothing then
                ( Button.icon (ToggleLogLine line) icon
                    |> Button.small
                    |> Button.subdued
                    |> Button.view
                , True
                )

            else
                ( UI.nothing, False )

        expanded =
            if isExpanded then
                div [ class "log-line_expanded" ] [ viewDataTable line.data ]

            else
                UI.nothing
    in
    div
        [ class "log-entry log-entry_log-line"
        , class ("log-line_" ++ LogLevel.toClassName_ line.level)
        , classList [ ( "log-line_expandable", expandable ) ]
        ]
        [ div [ class "log-line_collapsed" ]
            [ caret
            , LogLevel.view line.level
            , viewLoggedAt line.loggedAt
            , viewLogMessage line
            ]
        , expanded
        ]


viewDateBoundary : DateTime -> Html Msg
viewDateBoundary date =
    div [ class "log-entry log-entry_date-boundary" ]
        [ hr [ class "log-entry_date-boundary_date-divider" ] []
        , div [ class "log-entry_icon" ] [ Icon.view Icon.calendar ]
        , DateTime.view DateTime.ShortDate date
        , hr [ class "log-entry_date-boundary_date-divider" ] []
        ]


viewKeyedEntry : Model -> Int -> LogEntry -> ( String, Html Msg )
viewKeyedEntry model idx entry =
    let
        {- TODO: Use entry.id -}
        key d suffix =
            (d |> DateTime.toPosix |> Time.posixToMillis |> String.fromInt)
                ++ "_"
                ++ String.fromInt idx
                ++ "_"
                ++ suffix
    in
    case entry of
        Line line ->
            ( key line.loggedAt "line", lazy (viewLine model) line )

        DateBoundary date ->
            ( key date "boundary", lazy viewDateBoundary date )


id_ : String -> UUID.UUID
id_ s =
    UUID.forName s UUID.urlNamespace


newLines : List LogLine
newLines =
    [ { id = id_ "1690393751598", loggedAt = DateTime.fromPosix (Time.millisToPosix 1690393751598), message = Just "Newest", level = LogLevel.Info, data = Dict.empty }
    , { id = id_ "1690393747598", loggedAt = DateTime.fromPosix (Time.millisToPosix 1690393747598), message = Just "Newer", level = LogLevel.Info, data = Dict.empty }
    , { id = id_ "1690393744598", loggedAt = DateTime.fromPosix (Time.millisToPosix 1690393744598), message = Just "New", level = LogLevel.Info, data = Dict.empty }
    ]


oldLines : List LogLine
oldLines =
    [ { id = id_ "1690207705511", loggedAt = DateTime.fromPosix (Time.millisToPosix 1690207705511), message = Just "Old", level = LogLevel.Info, data = Dict.empty }
    , { id = id_ "1690207703512", loggedAt = DateTime.fromPosix (Time.millisToPosix 1690207703512), message = Just "Older", level = LogLevel.Info, data = Dict.empty }
    , { id = id_ "1690200685512", loggedAt = DateTime.fromPosix (Time.millisToPosix 1690200685512), message = Just "Oldest", level = LogLevel.Info, data = Dict.empty }
    ]


fauxLines : List LogLine
fauxLines =
    [ { id = id_ "1690392906913", loggedAt = DateTime.fromPosix (Time.millisToPosix 1690392906913), message = Just "GET /products?featured", level = LogLevel.Info, data = Dict.empty }
    , { id = id_ "1690392902913", loggedAt = DateTime.fromPosix (Time.millisToPosix 1690392902913), message = Nothing, level = LogLevel.Info, data = Dict.fromList [ ( "msg", "totally unstructured message" ), ( "with another", "message" ) ] }
    , { id = id_ "1690392899916", loggedAt = DateTime.fromPosix (Time.millisToPosix 1690392899916), message = Just "DB.getProducts returned 16 items in 59ms", level = LogLevel.Info, data = Dict.empty }
    , { id = id_ "1690392606916", loggedAt = DateTime.fromPosix (Time.millisToPosix 1690392606916), message = Nothing, level = LogLevel.Info, data = Dict.empty }
    , { id = id_ "1690392546916", loggedAt = DateTime.fromPosix (Time.millisToPosix 1690392546916), message = Just "16 times: DB.getProductDetails returned 1 item in 1240ms", level = LogLevel.Custom "TIMING", data = Dict.empty }
    , { id = id_ "1690392426916", loggedAt = DateTime.fromPosix (Time.millisToPosix 1690392426916), message = Just "POST /orders", level = LogLevel.Info, data = Dict.empty }
    , { id = id_ "1690392394916", loggedAt = DateTime.fromPosix (Time.millisToPosix 1690392394916), message = Just "DB.getUser returned 0 item in 35ms", level = LogLevel.Info, data = Dict.fromList [ ( "something", "hi" ), ( "and", "bye" ) ] }
    , { id = id_ "1690392378916", loggedAt = DateTime.fromPosix (Time.millisToPosix 1690392378916), message = Just "Request failed, couldn't find user", level = LogLevel.Error, data = Dict.empty }
    , { id = id_ "1690392186916", loggedAt = DateTime.fromPosix (Time.millisToPosix 1690392186916), message = Just "Add to cart", level = LogLevel.Warn, data = Dict.empty }
    , { id = id_ "1690306506916", loggedAt = DateTime.fromPosix (Time.millisToPosix 1690306506916), message = Just "Service Call", level = LogLevel.Info, data = Dict.empty }
    , { id = id_ "1690306326916", loggedAt = DateTime.fromPosix (Time.millisToPosix 1690306326916), message = Just "DB.getUser returned 1 item in 41ms", level = LogLevel.Custom "TIMING", data = Dict.empty }
    , { id = id_ "1690306324916", loggedAt = DateTime.fromPosix (Time.millisToPosix 1690306324916), message = Just "Order and User connected", level = LogLevel.Info, data = Dict.fromList [ ( "userId", "asd4swx1asd4swx1asd4swx1" ), ( "organization", "Apple Inc." ), ( "orderSize", "7" ) ] }
    , { id = id_ "1690299306916", loggedAt = DateTime.fromPosix (Time.millisToPosix 1690299306916), message = Just "log line", level = LogLevel.Warn, data = Dict.empty }
    ]


toEntries : Time.Zone -> List LogLine -> List LogEntry
toEntries timeZone lines =
    let
        f l ( entries, currentDate ) =
            case currentDate of
                Nothing ->
                    ( [ Line l, DateBoundary l.loggedAt ]
                    , Just l.loggedAt
                    )

                Just d ->
                    if DateTime.isSameDay timeZone l.loggedAt d then
                        ( entries ++ [ Line l ], Just l.loggedAt )

                    else
                        ( entries ++ [ Line l, DateBoundary l.loggedAt ]
                        , Just l.loggedAt
                        )
    in
    lines
        |> List.foldl f ( [], Nothing )
        |> Tuple.first


view : Env -> Model -> Html Msg
view env model =
    let
        lines =
            model.log.logLines
                |> toEntries env.timeZone
                |> List.indexedMap (viewKeyedEntry model)
    in
    div [ class "log" ]
        [ Html.Keyed.node "div" [ on "scroll" (ScrollEvent.decodeToMsg Scroll), class "log-entries" ] lines
        , div [ class "log_controls" ]
            [ Button.button FetchNew "Fetch New" |> Button.view
            , Button.button FetchOld "Fetch Old" |> Button.view
            ]
        ]
