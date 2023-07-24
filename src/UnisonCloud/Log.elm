module UnisonCloud.Log exposing (..)

import Dict
import Html exposing (Html, div, table, tbody, td, text, th, tr)
import Html.Attributes exposing (class, classList)
import Html.Events exposing (on)
import Json.Decode as Decode
import Set exposing (Set)
import Set.Extra as SetE
import Time
import UI
import UI.Button as Button
import UI.DateTime as DateTime exposing (DateTime)
import UI.Icon as Icon
import UI.Sizing as Sizing
import UnisonCloud.Env exposing (Env)
import UnisonCloud.LogEntry as LogEntry exposing (LogEntry)
import UnisonCloud.LogLevel as LogLevel



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
   viewport, but this requires you to know the total number of entries and the
   height of an entry (the latter not being a big deal).

   Instagram doesn't seem to work that way. By looking at the scroll bar when
   scroll you can see that it jumps, but the page itself remains smooth, this
   indicates to me that there isn't a large empty area prerendered, but really
   good scroll position maintenance.

   We could render the new elements off to the side, in an iframe, measure the
   sizes of everything before adding them such that we can calculate the new
   scroll position perfectly and make the switch without the user knowing.

-}


type alias LogEntries =
    List LogEntry


type Direction
    = Before
    | After



-- WHERE IS BOOKMARK?


type alias Model =
    { expandedEntries : Set String }


init : Env -> ( Model, Cmd Msg )
init _ =
    ( { expandedEntries = Set.empty }, Cmd.none )



-- UPDATE


type Msg
    = LogEntriesFetchFinished
    | Scroll
    | FetchMore
    | ToggleLogEntry LogEntry


update : Env -> Msg -> Model -> ( Model, Cmd Msg )
update _ msg model =
    case msg of
        ToggleLogEntry entry ->
            let
                loggedAt =
                    DateTime.toISO8601 entry.loggedAt
            in
            ( { model | expandedEntries = SetE.toggle loggedAt model.expandedEntries }, Cmd.none )

        _ ->
            ( model, Cmd.none )



-- HELPERS


logEntryHeight : Sizing.Rem
logEntryHeight =
    Sizing.Rem 2



-- EFFECTS


fetchLogEntries : Env -> LogEntry -> Direction -> Cmd Msg
fetchLogEntries _ _ _ =
    Cmd.none



-- VIEW


{-| If there's no message, print out the entry data instead of it is present,
finally, if there's no data, render an empty entry.

TODO: Add various highlights

-}
viewLogMessage : LogEntry -> Html Msg
viewLogMessage entry =
    let
        viewRawData =
            if LogEntry.hasData entry then
                entry
                    |> LogEntry.dataToList
                    |> List.map (\( k, v ) -> "\"" ++ k ++ "\": " ++ "\"" ++ v)
                    |> String.join ", "
                    |> (\d -> text ("{ " ++ d ++ " }"))
                    |> (\d -> div [ class "log-entry_log-message_raw-data" ] [ d ])

            else
                div [ class "log-entry_log-message_no-data" ] [ text "NO DATA" ]
    in
    case entry.message of
        Nothing ->
            viewRawData

        Just "" ->
            viewRawData

        Just m ->
            div [ class "log-entry_log-message_message" ] [ text m ]


viewDataTable : LogEntry.LogEntryData -> Html Msg
viewDataTable data =
    data
        |> Dict.toList
        |> List.map (\( k, v ) -> tr [] [ th [] [ text k ], td [] [ text v ] ])
        |> (\d -> table [ class "log-entry_log-message_data-table" ] [ tbody [] d ])


viewLoggedAt : DateTime -> Html Msg
viewLoggedAt dateTime =
    div [ class "log-entry_logged-at" ] [ DateTime.view DateTime.TimeWithSeconds dateTime ]


viewEntry : Model -> LogEntry -> Html Msg
viewEntry model entry =
    let
        isExpanded =
            Set.member (DateTime.toISO8601 entry.loggedAt) model.expandedEntries

        icon =
            if isExpanded then
                Icon.caretDown

            else
                Icon.caretRight

        ( caret, expandable ) =
            if LogEntry.hasData entry then
                ( Button.icon (ToggleLogEntry entry) icon
                    |> Button.small
                    |> Button.subdued
                    |> Button.view
                , True
                )

            else
                ( UI.nothing, False )

        expanded =
            if isExpanded then
                div [ class "log-entry_expanded" ] [ viewDataTable entry.data ]

            else
                UI.nothing
    in
    div [ class "log-entry", classList [ ( "log-entry_expandable", expandable ) ] ]
        [ div [ class "log-entry_collapsed" ]
            [ caret
            , LogLevel.view entry.level
            , viewLoggedAt entry.loggedAt
            , viewLogMessage entry
            ]
        , expanded
        ]


fauxEntries : List LogEntry
fauxEntries =
    [ { loggedAt = DateTime.fromPosix (Time.millisToPosix 1234), message = Just "log line", level = LogLevel.Info, data = Dict.empty }
    , { loggedAt = DateTime.fromPosix (Time.millisToPosix 12324), message = Nothing, level = LogLevel.Info, data = Dict.fromList [ ( "something", "hi" ), ( "and", "bye" ) ] }
    , { loggedAt = DateTime.fromPosix (Time.millisToPosix 12344), message = Just "log line", level = LogLevel.Info, data = Dict.empty }
    , { loggedAt = DateTime.fromPosix (Time.millisToPosix 123324), message = Nothing, level = LogLevel.Info, data = Dict.empty }
    , { loggedAt = DateTime.fromPosix (Time.millisToPosix 1234534), message = Just "log line", level = LogLevel.Info, data = Dict.empty }
    , { loggedAt = DateTime.fromPosix (Time.millisToPosix 123434), message = Just "log line", level = LogLevel.Info, data = Dict.empty }
    , { loggedAt = DateTime.fromPosix (Time.millisToPosix 124), message = Just "log line", level = LogLevel.Info, data = Dict.empty }
    , { loggedAt = DateTime.fromPosix (Time.millisToPosix 123344), message = Just "log line", level = LogLevel.Info, data = Dict.empty }
    , { loggedAt = DateTime.fromPosix (Time.millisToPosix 12334), message = Just "log line", level = LogLevel.Info, data = Dict.empty }
    , { loggedAt = DateTime.fromPosix (Time.millisToPosix 12), message = Just "log line", level = LogLevel.Info, data = Dict.empty }
    , { loggedAt = DateTime.fromPosix (Time.millisToPosix 15234), message = Just "log line", level = LogLevel.Info, data = Dict.empty }
    , { loggedAt = DateTime.fromPosix (Time.millisToPosix 188234), message = Just "log line", level = LogLevel.Info, data = Dict.empty }
    , { loggedAt = DateTime.fromPosix (Time.millisToPosix 129934), message = Just "log line", level = LogLevel.Info, data = Dict.empty }
    , { loggedAt = DateTime.fromPosix (Time.millisToPosix 127734), message = Just "log line", level = LogLevel.Info, data = Dict.empty }
    ]


view : Model -> Html Msg
view model =
    let
        entries =
            fauxEntries
                |> List.map (viewEntry model)
    in
    div [ on "scroll" (Decode.succeed Scroll), class "log" ] entries
