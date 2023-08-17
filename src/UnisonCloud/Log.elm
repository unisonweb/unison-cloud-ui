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
import RemoteData exposing (RemoteData(..), WebData)
import Set exposing (Set)
import Set.Extra as SetE
import UI
import UI.Button as Button
import UI.DateTime as DateTime exposing (DateTime)
import UI.Icon as Icon
import UI.Sizing as Sizing
import UI.Tooltip as Tooltip
import UnisonCloud.Api as CloudApi
import UnisonCloud.AppContext exposing (AppContext)
import UnisonCloud.FetchLogParams as FetchLogParams exposing (FetchLogParams)
import UnisonCloud.LogEntries as LogEntries exposing (LogEntry(..))
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


type LogBrowsingContext
    = ServiceContext ServiceId
    | ServiceDeployContext ServiceHash



-- WHERE IS BOOKMARK?


type alias Log =
    { expandedLines : Set String -- Set (LogId)
    , olderLogLines : WebData (List LogLine)
    , logLines : WebData (List LogLine)
    , newerLogLines : WebData (List LogLine)
    }


type alias Model =
    { log : Log }


init : AppContext -> LogBrowsingContext -> ( Model, Cmd Msg )
init appContext logBrowsingContext =
    ( { log =
            { expandedLines = Set.empty
            , olderLogLines = NotAsked
            , logLines = Loading
            , newerLogLines = NotAsked
            }
      }
    , fetchInitialLogLines appContext logBrowsingContext
    )



-- UPDATE


type Msg
    = FetchInitialLogLinesFinished (WebData (List LogLine))
    | FetchOlderLogLinesFinished (WebData (List LogLine))
    | FetchNewerLogLinesFinished (WebData (List LogLine))
    | Scroll ScrollEvent
    | ToggleLogLine LogLine


update : AppContext -> LogBrowsingContext -> Msg -> Model -> ( Model, Cmd Msg )
update appContext logBrowsingContext msg model =
    let
        log =
            model.log
    in
    case msg of
        FetchInitialLogLinesFinished logLines ->
            let
                log_ =
                    { log | logLines = logLines }
            in
            ( { model | log = log_ }, Cmd.none )

        FetchOlderLogLinesFinished olderLogLines ->
            let
                logLines =
                    log.logLines
                        |> RemoteData.map (\ls -> RemoteData.withDefault [] log.olderLogLines ++ ls)

                log_ =
                    { log | logLines = logLines, olderLogLines = olderLogLines }
            in
            ( { model | log = log_ }, Cmd.none )

        FetchNewerLogLinesFinished newerLogLines ->
            let
                logLines =
                    log.logLines
                        |> RemoteData.map (\ls -> ls ++ RemoteData.withDefault [] log.newerLogLines)

                log_ =
                    { log | logLines = logLines, newerLogLines = newerLogLines }
            in
            ( { model | log = log_ }, Cmd.none )

        Scroll ev ->
            let
                olderLogLines_ =
                    RemoteData.withDefault [] log.olderLogLines

                logLines_ =
                    RemoteData.withDefault [] log.logLines

                allLogLines =
                    olderLogLines_ ++ logLines_

                bookmark =
                    allLogLines
                        |> List.head
                        |> Maybe.map .loggedAt
            in
            case bookmark of
                Nothing ->
                    let
                        log_ =
                            { log | logLines = Loading }
                    in
                    ( { model | log = log_ }, fetchInitialLogLines appContext logBrowsingContext )

                Just bm ->
                    let
                        edgeOffset =
                            abs (ev.scrollHeight + ev.scrollTop - ev.clientHeight)

                        closenessOffset =
                            0

                        isCloseToEdge =
                            -- edgeOffset <= closenessOffset
                            ev.scrollTop == (ev.scrollHeight - ev.clientHeight)

                        ( log_, cmd ) =
                            if isCloseToEdge then
                                ( log, fetchOlderLogLines appContext logBrowsingContext bm )

                            else
                                ( log, Cmd.none )
                    in
                    ( { model | log = log_ }, cmd )

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


fetchInitialLogLines : AppContext -> LogBrowsingContext -> Cmd Msg
fetchInitialLogLines appContext logBrowsingContext =
    let
        params =
            FetchLogParams.fetchLogParams
    in
    fetchLogLines_ appContext logBrowsingContext params FetchInitialLogLinesFinished


fetchOlderLogLines : AppContext -> LogBrowsingContext -> DateTime -> Cmd Msg
fetchOlderLogLines appContext logBrowsingContext bookmark =
    let
        params =
            FetchLogParams.fetchLogParams
                |> FetchLogParams.withDirection FetchLogParams.Backward
                |> FetchLogParams.withEnd bookmark
    in
    fetchLogLines_ appContext logBrowsingContext params FetchOlderLogLinesFinished


fetchNewerLogLines : AppContext -> LogBrowsingContext -> DateTime -> Cmd Msg
fetchNewerLogLines appContext logBrowsingContext bookmark =
    let
        params =
            FetchLogParams.fetchLogParams
                |> FetchLogParams.withDirection FetchLogParams.Forward
                |> FetchLogParams.withStart bookmark
    in
    fetchLogLines_ appContext logBrowsingContext params FetchNewerLogLinesFinished


fetchLogLines_ :
    AppContext
    -> LogBrowsingContext
    -> FetchLogParams
    -> (WebData (List LogLine) -> Msg)
    -> Cmd Msg
fetchLogLines_ appContext logBrowsingContext params doneMsg =
    let
        params_ =
            FetchLogParams.withLimit 15 params

        endpoint =
            case logBrowsingContext of
                ServiceContext sid ->
                    CloudApi.serviceLogs sid params_

                ServiceDeployContext sh ->
                    CloudApi.serviceDeployLogs sh params_
    in
    endpoint
        |> HttpApi.toRequest (Decode.field "logs" (Decode.list LogLine.decode))
            (RemoteData.fromResult >> doneMsg)
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
    let
        content =
            Tooltip.text (DateTime.toISO8601 dateTime)

        trigger =
            -- div [ class "log-line_logged-at" ] [ DateTime.view DateTime.TimeWithSeconds dateTime ]
            div [ class "log-line_logged-at" ] [ text (DateTime.toISO8601 dateTime) ]
    in
    content
        |> Tooltip.tooltip
        |> Tooltip.view trigger


viewLine : Model -> Bool -> LogLine -> Html Msg
viewLine model isFresh line =
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
        , classList [ ( "log-line_expandable", expandable ), ( "log-line_fresh", isFresh ) ]
        ]
        [ div [ class "log-line_collapsed" ]
            [ caret
            , LogLevel.view line.level
            , text line.id
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


viewEntry : Model -> LogEntry -> Html Msg
viewEntry model entry =
    case entry of
        Line line ->
            viewLine model line.justFetched line.line

        DateBoundary date ->
            viewDateBoundary date


viewKeyedEntry : Model -> LogEntry -> ( String, Html Msg )
viewKeyedEntry model entry =
    let
        row =
            lazy (viewEntry model) entry

        key =
            case entry of
                Line line ->
                    line.line.id

                DateBoundary date ->
                    "boundary-" ++ DateTime.toISO8601 date
    in
    ( key, row )


view : AppContext -> Model -> Html Msg
view appContext model =
    let
        older =
            RemoteData.withDefault [] model.log.olderLogLines |> List.map (\l -> { justFetched = True, line = l })

        current =
            RemoteData.withDefault [] model.log.logLines |> List.map (\l -> { justFetched = False, line = l })

        newer =
            RemoteData.withDefault [] model.log.newerLogLines |> List.map (\l -> { justFetched = True, line = l })

        allLogLines =
            older ++ current ++ newer

        lines =
            allLogLines
                |> LogEntries.fromLines appContext.timeZone
                |> List.map (viewKeyedEntry model)
    in
    div [ class "log" ]
        [ Html.Keyed.node "div" [ on "scroll" (ScrollEvent.decodeToMsg Scroll), class "log-entries" ] lines ]
