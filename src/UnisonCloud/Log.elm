module UnisonCloud.Log exposing (..)

import Dict
import Html exposing (Html, div, hr, table, tbody, td, text, th, tr)
import Html.Attributes exposing (class, classList)
import Html.Events exposing (on)
import Html.Keyed
import Html.Lazy exposing (lazy)
import Json.Decode as Decode
import Lib.HttpApi as HttpApi
import Lib.ScrollEvent as ScrollEvent exposing (ScrollEvent)
import Lib.Util
import RemoteData exposing (RemoteData(..), WebData)
import Set exposing (Set)
import Set.Extra as SetE
import Time
import UI
import UI.Button as Button
import UI.DateTime as DateTime exposing (DateTime)
import UI.Icon as Icon
import UI.Sizing as Sizing
import UnisonCloud.Api as CloudApi
import UnisonCloud.AppContext exposing (AppContext)
import UnisonCloud.LogLevel as LogLevel
import UnisonCloud.LogLine as LogLine exposing (LogLine)
import UnisonCloud.Service exposing (ServiceId)
import UnisonCloud.ServiceHash exposing (ServiceHash)



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


type LogBrowsingContext
    = ServiceContext ServiceId
    | ServiceDeployContext ServiceHash



-- WHERE IS BOOKMARK?


type alias Log =
    { expandedLines : Set String -- Set (LogId)
    , freshOldLines : WebData (List LogLine)
    , logLines : WebData (List LogLine)
    , freshNewLines : WebData (List LogLine)
    }


type alias Model =
    { log : Log }


init : AppContext -> LogBrowsingContext -> ( Model, Cmd Msg )
init appContext logBrowsingContext =
    ( { log =
            { expandedLines = Set.empty
            , freshOldLines = NotAsked
            , logLines = Loading
            , freshNewLines = NotAsked
            }
      }
    , fetchLogLines appContext logBrowsingContext
    )



-- UPDATE


type Msg
    = FetchLogLinesFinished (WebData (List LogLine))
    | Scroll ScrollEvent
    | FetchNew
    | FetchOld
    | ToggleLogLine LogLine


update : AppContext -> Msg -> Model -> ( Model, Cmd Msg )
update _ msg model =
    let
        log =
            model.log
    in
    case msg of
        FetchLogLinesFinished logLines ->
            let
                log_ =
                    { log | logLines = RemoteData.map List.reverse logLines }
            in
            ( { model | log = log_ }, Cmd.none )

        FetchNew ->
            ( model, Cmd.none )

        {-
           let
               log_ =
                   { log
                       | logLines = log.freshNewLines ++ log.logLines
                       , freshNewLines = newLines
                   }
           in
           ( { model | log = log_ }, Cmd.none )
        -}
        FetchOld ->
            ( model, Cmd.none )

        {-
           let
               log_ =
                   { log
                       | logLines = log.logLines ++ log.freshOldLines
                       , freshOldLines = oldLines
                   }
           in
           ( { model | log = log_ }, Cmd.none )
        -}
        Scroll _ ->
            {-
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
            -}
            ( model, Cmd.none )

        ToggleLogLine line ->
            let
                log_ =
                    { log | expandedLines = SetE.toggle line.id log.expandedLines }
            in
            ( { model | log = log_ }, Cmd.none )



-- HELPERS


logEntryHeight : Sizing.Rem
logEntryHeight =
    Sizing.Rem 1.5



-- EFFECTS


fetchLogLines : AppContext -> LogBrowsingContext -> Cmd Msg
fetchLogLines appContext logBrowsingContext =
    let
        endpoint =
            case logBrowsingContext of
                ServiceContext sid ->
                    CloudApi.serviceLogs sid

                ServiceDeployContext sh ->
                    CloudApi.serviceDeployLogs sh
    in
    endpoint
        |> HttpApi.toRequest (Decode.field "logs" (Decode.list LogLine.decode))
            (RemoteData.fromResult >> FetchLogLinesFinished)
        |> HttpApi.perform appContext.api



-- VIEW


{-| If there's no message, print out the line data instead of it is present,
finally, if there's no data, render an empty line.

TODO:Add various highlights, like bolding of GET and POST.

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


view : AppContext -> Model -> Html Msg
view appContext model =
    let
        err =
            case model.log.logLines of
                Failure e ->
                    div [] [ text (Lib.Util.httpErrorToString e) ]

                _ ->
                    UI.nothing

        lines =
            model.log.logLines
                |> RemoteData.withDefault []
                |> toEntries appContext.timeZone
                |> List.indexedMap (viewKeyedEntry model)
    in
    div [ class "log" ]
        [ Html.Keyed.node "div" [ on "scroll" (ScrollEvent.decodeToMsg Scroll), class "log-entries" ] lines
        , div [ class "log_controls" ]
            [ err
            , Button.button FetchNew "Fetch New" |> Button.view
            , Button.button FetchOld "Fetch Old" |> Button.view
            ]
        ]
