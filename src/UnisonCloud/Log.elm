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
import Debounce exposing (Debounce)
import Dict
import Html
    exposing
        ( Html
        , abbr
        , details
        , div
        , h2
        , hr
        , p
        , span
        , summary
        , table
        , tbody
        , td
        , text
        , th
        , tr
        )
import Html.Attributes exposing (class, classList, id, style)
import Html.Events exposing (on)
import Html.Keyed
import Html.Lazy exposing (lazy)
import Http
import Json.Decode as Decode
import Lib.HttpApi as HttpApi
import Lib.ScrollEvent as ScrollEvent exposing (ScrollEvent)
import Lib.UserHandle as UserHandle
import Lib.Util as Util
import List.Extra as ListE
import ProdDebug exposing (debugLog)
import RemoteData exposing (RemoteData(..), WebData)
import Set exposing (Set)
import Set.Extra as SetE
import String.Extra as StringE
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
import UnisonCloud.Account as Account
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
    { log : Log
    , modal : Modal
    , debounce : Debounce (Cmd Msg)
    }


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
      , debounce = Debounce.init
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


{-| This defines how the debouncer should work.
Choose the strategy for your use case.
-}
debounceConfig : Debounce.Config Msg
debounceConfig =
    { strategy = Debounce.later 500
    , transform = DebounceMsg
    }


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
    | DebounceMsg Debounce.Msg


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
            ( { model | log = log_ }
            , Cmd.batch
                [ debugLog
                    ("Fetched initial log lines: "
                        ++ (logLines_
                                |> RemoteData.map List.length
                                |> RemoteData.withDefault 0
                                |> String.fromInt
                           )
                    )
                , Util.delayMsg pollingInterval RequestToFetchNewestLogLines
                ]
            )

        FetchOlderLogLinesFinished olderLogLines ->
            let
                logLines =
                    log.logLines
                        |> RemoteData.map (\ls -> RemoteData.withDefault [] log.olderLogLines ++ ls)

                log_ =
                    { log | logLines = logLines, olderLogLines = olderLogLines }
            in
            ( { model | log = log_ }
            , Cmd.batch
                [ debugLog
                    ("Fetched older log lines: "
                        ++ (olderLogLines
                                |> RemoteData.map List.length
                                |> RemoteData.withDefault 0
                                |> String.fromInt
                           )
                    )
                ]
            )

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
                            ( l
                            , Cmd.batch
                                [ ProdDebug.debugLog "fetching newest lines"
                                , fetchNewestLogLines appContext logBrowsingContext loggedAt
                                ]
                            )

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
            ( { model | log = log_ }
            , Cmd.batch
                [ debugLog
                    ("Fetched newer log lines: "
                        ++ (lines_
                                |> RemoteData.map List.length
                                |> RemoteData.withDefault 0
                                |> String.fromInt
                           )
                    )
                , Util.delayMsg pollingInterval RequestToFetchNewestLogLines
                ]
            )

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

                scroll =
                    ProdDebug.debugLog "Scroll"
            in
            case bookmark of
                Nothing ->
                    let
                        log_ =
                            { log | logLines = Loading }
                    in
                    ( { model | log = log_ }
                    , Cmd.batch [ scroll, fetchInitialLogLines appContext logBrowsingContext ]
                    )

                Just bm ->
                    let
                        edgeOffset =
                            abs (ev.scrollHeight + ev.scrollTop - ev.clientHeight)

                        logRowHeight =
                            24

                        closenessOffset =
                            3 * logRowHeight

                        isCloseToEdge =
                            edgeOffset <= closenessOffset

                        ( log_, debounce, cmd ) =
                            if isCloseToEdge then
                                let
                                    ( debounce_, debounceCmd ) =
                                        Debounce.push debounceConfig
                                            (Cmd.batch
                                                [ ProdDebug.debugLog "Fetching old lines"
                                                , fetchOlderLogLines
                                                    appContext
                                                    logBrowsingContext
                                                    bm
                                                ]
                                            )
                                            model.debounce
                                in
                                ( log
                                , debounce_
                                , Cmd.batch
                                    [ ProdDebug.debugLog "within edge window"
                                    , debounceCmd
                                    ]
                                )

                            else
                                ( log, model.debounce, Cmd.none )
                    in
                    ( { model | log = log_, debounce = debounce }, Cmd.batch [ scroll, cmd ] )

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

        DebounceMsg msg_ ->
            let
                ( debounce, cmd ) =
                    Debounce.update
                        debounceConfig
                        (Debounce.takeLast identity)
                        msg_
                        model.debounce
            in
            ( { model | debounce = debounce }
            , cmd
            )



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

        now =
            appContext.now

        endpoint =
            case logBrowsingContext of
                ServiceContext name ->
                    CloudApi.serviceLogs now appContext.session.handle name params_

                ServiceDeployContext sh ->
                    CloudApi.serviceDeployLogs now sh params_

        decodeLogs =
            Decode.map
                LogLine.decodeList
                (Decode.field "logs" (Decode.list Decode.string))
    in
    endpoint
        |> HttpApi.toRequest decodeLogs
            (RemoteData.fromResult >> doneMsg)
        |> HttpApi.perform appContext.api



-- VIEW


{-| If there's no message, print out the line data instead of it is present,
finally, if there's no data, render an empty line.

TODO:Add various highlights, like bolding of GET and POST.

-}
viewLogMessage : Tooltip.Position -> LogLine -> Html Msg
viewLogMessage tooltipPosition line =
    let
        viewRawData =
            if LogLine.hasData line then
                line
                    |> LogLine.dataToList
                    |> List.map (\( k, v ) -> "\"" ++ k ++ "\": " ++ "\"" ++ v ++ "\"")
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

        Just message ->
            let
                words =
                    message
                        |> String.split " "
                        |> List.map (viewTruncated 72 tooltipPosition "log-line-message_truncated-word")
                        |> List.intersperse (text " ")
            in
            div [ class "log-line_log-message_message" ] words


viewTruncated : Int -> Tooltip.Position -> String -> String -> Html msg
viewTruncated maxLength tooltipPosition className word =
    if String.length word > maxLength then
        let
            content =
                Tooltip.text (StringE.wrap maxLength word)

            trigger =
                abbr [ class className ] [ text (StringE.ellipsis maxLength word) ]
        in
        content
            |> Tooltip.tooltip
            |> Tooltip.withPosition tooltipPosition
            |> Tooltip.withArrow Tooltip.Start
            |> Tooltip.view trigger

    else
        text word


viewDataTable : Tooltip.Position -> LogLine.LogLineData -> Html Msg
viewDataTable tooltipPosition data =
    let
        key k =
            viewTruncated 12
                tooltipPosition
                "log-line_log-message_data-table_truncated-key"
                k

        value v =
            v
                |> String.split " "
                |> List.map
                    (viewTruncated
                        56
                        tooltipPosition
                        "log-line_log-message_data-table_truncated-value-word"
                    )
                |> List.intersperse (text " ")
    in
    data
        |> Dict.toList
        |> List.map (\( k, v ) -> tr [] [ th [] [ key k ], td [] (value v) ])
        |> (\d -> table [ class "log-line_log-message_data-table" ] [ tbody [] d ])


viewLoggedAt : Time.Zone -> Tooltip.Position -> DateTime -> Html Msg
viewLoggedAt zone tooltipPosition dateTime =
    let
        content =
            Tooltip.text (DateTime.toString DateTime.FullDateTime zone dateTime)

        trigger =
            div [ class "log-line_logged-at" ]
                [ text (DateTime.toString DateTime.TimeWithSeconds24Hour zone dateTime) ]
    in
    content
        |> Tooltip.tooltip
        |> Tooltip.withPosition tooltipPosition
        |> Tooltip.withArrow Tooltip.Start
        |> Tooltip.view trigger


viewLine : Time.Zone -> Model -> Tooltip.Position -> LogLine -> Html Msg
viewLine zone model tooltipPosition line =
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
                div [ class "log-line_expanded" ]
                    [ viewDataTable tooltipPosition line.data
                    ]

            else
                UI.nothing

        coloredLogId =
            let
                shortId =
                    line.id |> String.split "-" |> List.head |> Maybe.withDefault ""

                hexColor =
                    shortId |> String.left 6
            in
            span [ style "background" ("#" ++ hexColor) ] [ text shortId ]
    in
    div
        [ class "log-entry log-entry_log-line"
        , class ("log-line_" ++ LogLevel.toClassName_ line.level)
        , classList [ ( "log-line_expandable", expandable ) ]
        ]
        [ div [ class "log-line_collapsed" ]
            [ caret
            , LogLevel.view line.level
            , viewLoggedAt zone tooltipPosition line.loggedAt
            , coloredLogId
            , viewLogMessage tooltipPosition line
            ]
        , expanded
        ]


viewDateBoundary : Time.Zone -> DateTime -> Html Msg
viewDateBoundary zone date =
    div [ class "log-entry log-entry_date-boundary" ]
        [ hr [ class "log-entry_date-boundary_date-divider" ] []
        , div [ class "log-entry_icon" ] [ Icon.view Icon.calendar ]
        , DateTime.view DateTime.ShortDate zone date
        , hr [ class "log-entry_date-boundary_date-divider" ] []
        ]


viewEntry : Time.Zone -> Model -> Int -> LogEntry -> Html Msg
viewEntry zone model index entry =
    let
        isLastEntry =
            -- we're going backwards through the list so the first entry in the list is the last in the UI.
            index == 0

        tooltipPosition =
            if isLastEntry then
                Tooltip.Above

            else
                Tooltip.Below
    in
    case entry of
        Line line ->
            viewLine zone model tooltipPosition line

        DateBoundary date ->
            viewDateBoundary zone date


viewKeyedEntry : Time.Zone -> Model -> Int -> LogEntry -> ( String, Html Msg )
viewKeyedEntry zone model index entry =
    let
        row =
            lazy (viewEntry zone model index) entry

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
                    model.log
                        |> logLinesOldestToNewest
                        |> LogEntries.fromLines timeZone
                        |> List.indexedMap (viewKeyedEntry timeZone model)
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

        Failure e ->
            let
                errorDetails =
                    if Account.isOrganizationMember (UserHandle.unsafeFromString "unison") appContext.session then
                        details [] [ summary [] [ text "Error Details" ], div [] [ text (Util.httpErrorToString e) ] ]

                    else
                        UI.nothing
            in
            ( div []
                [ text "Something went wrong in fetching the logs"
                , errorDetails
                ]
            , Nothing
            )
