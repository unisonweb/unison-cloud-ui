{-

   Infinite Scroll Challenges and Goals
   ------------------------------------

   * There should not be any Jank when scrolling up to look at older log messages
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


module UnisonCloud.Log exposing (..)

import Browser.Dom as Dom
import Dict
import Html exposing (Html, div, h2, hr, p, table, tbody, td, text, th, tr)
import Html.Attributes exposing (class, classList, id)
import Html.Events exposing (on)
import Html.Keyed
import Html.Lazy exposing (lazy)
import Http
import Json.Decode as Decode
import Lib.HttpApi as HttpApi
import Lib.ScrollEvent as ScrollEvent exposing (ScrollEvent)
import Lib.Util as Util
import List.Extra as ListE
import RemoteData exposing (RemoteData(..), WebData)
import Set exposing (Set)
import Set.Extra as SetE
import Task
import Time
import UI
import UI.Button as Button
import UI.Card as Card
import UI.DateTime as DateTime exposing (DateTime)
import UI.EmptyState as EmptyState
import UI.EmptyStateCard as EmptyStateCard
import UI.Icon as Icon
import UI.Modal as Modal
import UI.Nudge as Nudge
import UI.Placeholder as Placeholder
import UI.Sizing as Sizing
import UI.Tooltip as Tooltip
import UnisonCloud.Api as CloudApi
import UnisonCloud.AppContext exposing (AppContext)
import UnisonCloud.FetchLogParams as FetchLogParams exposing (FetchLogParams)
import UnisonCloud.Link as Link
import UnisonCloud.LogEntries as LogEntries exposing (LogEntry(..))
import UnisonCloud.LogLevel as LogLevel
import UnisonCloud.LogLine as LogLine exposing (LogLine)
import UnisonCloud.Service.ServiceName exposing (ServiceName)
import UnisonCloud.ServiceHash exposing (ServiceHash)



-- MODEL


type LogBrowsingContext
    = ServiceContext ServiceName
    | ServiceDeployContext ServiceHash


type Modal
    = NoModal
    | GetStartedWithLoggingModal


type alias Log =
    { expandedLines : Set String -- Set (LogId)
    , olderLogLines : WebData (List LogLine)
    , logLines : WebData (List LogLine)
    , newestLogLines : WebData (List LogLine)
    , offScreenNewestLogLines : WebData (List LogLine)
    }


type alias Model =
    { log : Log, modal : Modal }


init : AppContext -> LogBrowsingContext -> ( Model, Cmd Msg )
init appContext logBrowsingContext =
    ( { log =
            { expandedLines = Set.empty
            , olderLogLines = NotAsked
            , logLines = Loading
            , newestLogLines = NotAsked
            , offScreenNewestLogLines = NotAsked
            }
      , modal = NoModal
      }
    , fetchInitialLogLines appContext logBrowsingContext
    )



-- CONFIG


pollingInterval : Float
pollingInterval =
    -- 7.5 seconds
    7500


pageSize : Int
pageSize =
    30



-- UPDATE


type Msg
    = NoOp
    | FetchInitialLogLinesFinished (WebData (List LogLine))
    | FetchOlderLogLinesFinished (WebData (List LogLine))
    | FetchNewestLogLinesFinished (WebData (List LogLine))
    | RequestToFetchNewestLogLines
    | Scroll ScrollEvent
    | ToggleLogLine LogLine
    | RevealNewOffscreenLogLines
    | ShowGetStartedWithLoggingModal
    | CloseModal


update : AppContext -> LogBrowsingContext -> Msg -> Model -> ( Model, Cmd Msg )
update appContext logBrowsingContext msg model =
    let
        log =
            model.log
    in
    case msg of
        NoOp ->
            ( model, Cmd.none )

        FetchInitialLogLinesFinished logLines ->
            let
                logLines_ =
                    case logLines of
                        Failure (Http.BadStatus 404) ->
                            Success []

                        _ ->
                            logLines

                log_ =
                    { log | logLines = logLines_ }
            in
            ( { model | log = log_ }, Util.delayMsg pollingInterval RequestToFetchNewestLogLines )

        FetchOlderLogLinesFinished olderLogLines ->
            let
                logLines =
                    log.logLines
                        |> RemoteData.map (\ls -> RemoteData.withDefault [] log.olderLogLines ++ ls)

                log_ =
                    { log | logLines = logLines, olderLogLines = olderLogLines }
            in
            ( { model | log = log_ }, Cmd.none )

        RequestToFetchNewestLogLines ->
            let
                ( log_, cmd ) =
                    case newestLoggedAt model.log of
                        Just loggedAt ->
                            let
                                l =
                                    case log.offScreenNewestLogLines of
                                        Success _ ->
                                            log

                                        _ ->
                                            { log | offScreenNewestLogLines = Loading }
                            in
                            ( l, fetchNewestLogLines appContext logBrowsingContext loggedAt )

                        _ ->
                            ( log, Cmd.none )
            in
            ( { model | log = log_ }, cmd )

        FetchNewestLogLinesFinished lines ->
            let
                allLogIds =
                    log
                        |> logLinesOldestToNewest
                        |> List.map .id

                lines_ =
                    lines
                        |> RemoteData.map (List.filter (\l -> not (List.member l.id allLogIds)))

                log_ =
                    { log | offScreenNewestLogLines = lines_ }
            in
            ( { model | log = log_ }, Util.delayMsg pollingInterval RequestToFetchNewestLogLines )

        Scroll ev ->
            let
                olderLogLines_ =
                    RemoteData.withDefault [] log.olderLogLines

                logLines_ =
                    RemoteData.withDefault [] log.logLines

                allLogLines_ =
                    olderLogLines_ ++ logLines_

                bookmark =
                    allLogLines_
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
                            edgeOffset <= closenessOffset

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

        RevealNewOffscreenLogLines ->
            let
                logLines =
                    case ( log.logLines, log.newestLogLines ) of
                        ( Success ll, Success nl ) ->
                            Success (ll ++ nl)

                        _ ->
                            log.logLines

                log_ =
                    { log
                        | logLines = logLines
                        , newestLogLines = log.offScreenNewestLogLines
                        , offScreenNewestLogLines = NotAsked
                    }

                cmd =
                    Dom.getViewportOf
                        "log-entries"
                        |> Task.andThen (.scene >> .height >> Dom.setViewportOf "log-entries" 0)
                        |> Task.attempt (always NoOp)
            in
            ( { model | log = log_ }, cmd )

        ShowGetStartedWithLoggingModal ->
            ( { model | modal = GetStartedWithLoggingModal }, Cmd.none )

        CloseModal ->
            ( { model | modal = NoModal }, Cmd.none )



-- HELPERS


oldestLoggedAt : Log -> Maybe DateTime
oldestLoggedAt log =
    log
        |> logLinesOldestToNewest
        |> List.head
        |> Maybe.map .loggedAt


newestLoggedAt : Log -> Maybe DateTime
newestLoggedAt log =
    log
        |> logLinesOldestToNewest
        |> ListE.last
        |> Maybe.map .loggedAt


logLinesOldestToNewest : Log -> List LogLine
logLinesOldestToNewest log =
    let
        older =
            RemoteData.withDefault [] log.olderLogLines

        current =
            RemoteData.withDefault [] log.logLines

        newest =
            RemoteData.withDefault [] log.newestLogLines
    in
    older ++ current ++ newest


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


fetchNewestLogLines : AppContext -> LogBrowsingContext -> DateTime -> Cmd Msg
fetchNewestLogLines appContext logBrowsingContext bookmark =
    let
        params =
            FetchLogParams.fetchLogParams
                |> FetchLogParams.withDirection FetchLogParams.Forward
                |> FetchLogParams.withStart bookmark
    in
    fetchLogLines_ appContext logBrowsingContext params FetchNewestLogLinesFinished


fetchLogLines_ :
    AppContext
    -> LogBrowsingContext
    -> FetchLogParams
    -> (WebData (List LogLine) -> Msg)
    -> Cmd Msg
fetchLogLines_ appContext logBrowsingContext params doneMsg =
    let
        params_ =
            FetchLogParams.withLimit pageSize params

        endpoint =
            case logBrowsingContext of
                ServiceContext name ->
                    CloudApi.serviceLogs appContext.session.handle name params_

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


viewLoggedAt : Time.Zone -> DateTime -> Html Msg
viewLoggedAt zone dateTime =
    let
        content =
            Tooltip.text (DateTime.toString DateTime.FullDateTime zone dateTime)

        trigger =
            div [ class "log-line_logged-at" ]
                [ text (DateTime.toString DateTime.TimeWithSeconds24Hour zone dateTime) ]
    in
    content
        |> Tooltip.tooltip
        |> Tooltip.withArrow Tooltip.Start
        |> Tooltip.view trigger


viewLine : Time.Zone -> Model -> LogLine -> Html Msg
viewLine zone model line =
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
            , viewLoggedAt zone line.loggedAt
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


viewEntry : Time.Zone -> Model -> LogEntry -> Html Msg
viewEntry zone model entry =
    case entry of
        Line line ->
            viewLine zone model line

        DateBoundary date ->
            viewDateBoundary date


viewKeyedEntry : Time.Zone -> Model -> LogEntry -> ( String, Html Msg )
viewKeyedEntry zone model entry =
    let
        row =
            lazy (viewEntry zone model) entry

        key =
            case entry of
                Line line ->
                    line.id

                DateBoundary date ->
                    "boundary-" ++ DateTime.toISO8601 date
    in
    ( key, row )


viewLoading : Html msg
viewLoading =
    let
        placeholder_ length intensity =
            Placeholder.text |> Placeholder.withLength length |> Placeholder.withIntensity intensity |> Placeholder.view

        placeholders =
            [ placeholder_ Placeholder.Medium Placeholder.Normal
            , placeholder_ Placeholder.Small Placeholder.Subdued
            , placeholder_ Placeholder.Large Placeholder.Subdued
            , placeholder_ Placeholder.Medium Placeholder.Subdued
            , placeholder_ Placeholder.Medium Placeholder.Normal
            , placeholder_ Placeholder.Small Placeholder.Subdued
            , placeholder_ Placeholder.Large Placeholder.Subdued
            , placeholder_ Placeholder.Medium Placeholder.Subdued
            ]
    in
    div [ class "log" ]
        [ div [ class "log-entries log-entries_loading" ]
            ((placeholders ++ placeholders ++ placeholders ++ placeholders)
                |> List.map (\p -> div [ class "log-entry_loading" ] [ p ])
            )
        ]


viewEmptyState : Html Msg
viewEmptyState =
    EmptyState.iconCloud
        (EmptyState.CircleCenterPiece (text "🪵"))
        |> EmptyState.withContent
            [ h2 [] [ text "Nothing's been logged yet" ]
            , p [] [ text "Logs will show up here as the service is called." ]
            , Button.iconThenLabel
                ShowGetStartedWithLoggingModal
                Icon.graduationCap
                "Get started with logging"
                |> Button.decorativeBlue
                |> Button.view
            ]
        |> EmptyStateCard.view_ Card.SurfaceBackground


viewGetStartedWithLoggingModal : Modal.Modal Msg
viewGetStartedWithLoggingModal =
    let
        getStarted =
            """info "beginning transmogrification..." []
warn "operation failed, ignoring" [("name", "bob"), ("fruit", "🍍")]
"""

        content =
            div []
                [ p []
                    [ text "Log messages can be arbitrary JSON, using the low level functions "
                    , UI.inlineCode [] (text "Log.json")
                    , text "and"
                    , UI.inlineCode [] (text "Log.lazyJson")
                    , text "but there are convenience functions for common cases. Unison Cloud's log viewer is set up to nicely render these. Here's a short example:"
                    ]
                , UI.codeBlock [] (text getStarted)
                ]
    in
    content
        |> Modal.content
        |> Modal.modal "log_get-started-with-logging-modal" CloseModal
        |> Modal.withHeader "Get started with logging"
        |> Modal.withLeftSideFooter
            [ div []
                [ text "Learn more in the "
                , Link.view "Cloud project documentation." Link.cloudDocs
                ]
            ]
        |> Modal.withActions
            [ Button.iconThenLabel CloseModal Icon.thumbsUp "Got It"
                |> Button.emphasized
            ]


view : AppContext -> Model -> ( Html Msg, Maybe (Modal.Modal Msg) )
view appContext model =
    let
        timeZone =
            appContext.timeZone

        modal =
            case model.modal of
                NoModal ->
                    Nothing

                GetStartedWithLoggingModal ->
                    Just viewGetStartedWithLoggingModal
    in
    case model.log.logLines of
        NotAsked ->
            ( viewLoading, Nothing )

        Loading ->
            ( viewLoading, Nothing )

        Success _ ->
            let
                offscreenLines =
                    case model.log.offScreenNewestLogLines of
                        Success [] ->
                            UI.nothing

                        Success ls ->
                            div [ class "log_reveal-new-offscreen-log-lines" ]
                                [ Button.iconThenLabel RevealNewOffscreenLogLines Icon.arrowDown "Reveal new entries"
                                    |> Button.small
                                    |> Button.emphasized
                                    |> Button.view
                                , Nudge.nudge |> Nudge.withNumber (List.length ls) |> Nudge.view
                                ]

                        _ ->
                            UI.nothing

                lines =
                    logLinesOldestToNewest model.log
                        |> LogEntries.fromLines timeZone
                        |> List.map (viewKeyedEntry timeZone model)
            in
            case lines of
                [] ->
                    ( viewEmptyState, modal )

                _ ->
                    ( div [ class "log" ]
                        [ Html.Keyed.node "div"
                            [ id "log-entries"
                            , on "scroll" (ScrollEvent.decodeToMsg Scroll)
                            , class "log-entries"
                            ]
                            lines
                        , offscreenLines
                        ]
                    , modal
                    )

        Failure _ ->
            ( div [] [ text "Something went wrong in fetching the logs" ], Nothing )
