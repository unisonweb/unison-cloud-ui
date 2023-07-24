module UnisonCloud.Log exposing (..)

import Dict
import Html exposing (Html, div, text)
import Html.Attributes exposing (class)
import Html.Events exposing (on)
import Http
import Json.Decode as Decode
import Time
import UI.DateTime as DateTime exposing (DateTime)
import UI.Icon as Icon
import UI.Sizing as Sizing
import UnisonCloud.Env as Env exposing (Env)
import UnisonCloud.LogEntry exposing (LogEntry)
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



-- WHERE IS BOOKMARK?


type Log
    = NotAsked
    | FirstTimeLoading
    | Loading LogEntries
    | Success LogEntries
    | Failure Http.Error


type Direction
    = Before
    | After


type alias Model =
    Log


init : Env -> ( Model, Cmd Msg )
init _ =
    ( NotAsked, Cmd.none )



-- UPDATE


type Msg
    = LogEntriesFetchFinished
    | Scroll
    | FetchMore


update : Env -> Msg -> Model -> ( Model, Cmd Msg )
update _ _ model =
    ( model, Cmd.none )



-- HELPERS


logEntryHeight : Sizing.Rem
logEntryHeight =
    Sizing.Rem 2



-- EFFECTS


fetchLogEntries : Env -> LogEntry -> Direction -> Cmd Msg
fetchLogEntries env bookmark direction =
    Cmd.none



-- VIEW


{-| If there's no message, print out the entry data instead of it is present,
finally, if there's no data, render an empty entry.

TODO: Add various highlights

-}
viewLogMessage : LogEntry -> Html Msg
viewLogMessage entry =
    let
        viewRawData data =
            if Dict.isEmpty data then
                div [ class "log-entry_log-message_no-data" ] [ text "NO DATA" ]

            else
                data
                    |> Dict.toList
                    |> List.map (\( k, v ) -> "\"" ++ k ++ "\": " ++ "\"" ++ v)
                    |> String.join ", "
                    |> (\d -> text ("{ " ++ d ++ " }"))
                    |> (\d -> div [ class "log-entry_log-message_raw-data" ] [ d ])
    in
    case entry.message of
        Nothing ->
            viewRawData entry.data

        Just "" ->
            viewRawData entry.data

        Just m ->
            div [ class "log-entry_log-message_message" ] [ text m ]


viewLoggedAt : DateTime -> Html Msg
viewLoggedAt dateTime =
    div [ class "log-entry_logged-at" ] [ DateTime.view DateTime.TimeWithSeconds dateTime ]


viewEntry : LogEntry -> Html Msg
viewEntry entry =
    div [ class "log-entry" ]
        [ LogLevel.view entry.level
        , viewLoggedAt entry.loggedAt
        , viewLogMessage entry
        ]


fauxEntries : List LogEntry
fauxEntries =
    [ { loggedAt = DateTime.fromPosix (Time.millisToPosix 1234), message = Just "log line", level = LogLevel.Info, data = Dict.empty }
    , { loggedAt = DateTime.fromPosix (Time.millisToPosix 1234), message = Nothing, level = LogLevel.Info, data = Dict.fromList [ ( "something", "hi" ), ( "and", "bye" ) ] }
    , { loggedAt = DateTime.fromPosix (Time.millisToPosix 1234), message = Just "log line", level = LogLevel.Info, data = Dict.empty }
    , { loggedAt = DateTime.fromPosix (Time.millisToPosix 1234), message = Nothing, level = LogLevel.Info, data = Dict.empty }
    , { loggedAt = DateTime.fromPosix (Time.millisToPosix 1234), message = Just "log line", level = LogLevel.Info, data = Dict.empty }
    , { loggedAt = DateTime.fromPosix (Time.millisToPosix 1234), message = Just "log line", level = LogLevel.Info, data = Dict.empty }
    , { loggedAt = DateTime.fromPosix (Time.millisToPosix 1234), message = Just "log line", level = LogLevel.Info, data = Dict.empty }
    , { loggedAt = DateTime.fromPosix (Time.millisToPosix 1234), message = Just "log line", level = LogLevel.Info, data = Dict.empty }
    , { loggedAt = DateTime.fromPosix (Time.millisToPosix 1234), message = Just "log line", level = LogLevel.Info, data = Dict.empty }
    , { loggedAt = DateTime.fromPosix (Time.millisToPosix 1234), message = Just "log line", level = LogLevel.Info, data = Dict.empty }
    , { loggedAt = DateTime.fromPosix (Time.millisToPosix 1234), message = Just "log line", level = LogLevel.Info, data = Dict.empty }
    , { loggedAt = DateTime.fromPosix (Time.millisToPosix 1234), message = Just "log line", level = LogLevel.Info, data = Dict.empty }
    , { loggedAt = DateTime.fromPosix (Time.millisToPosix 1234), message = Just "log line", level = LogLevel.Info, data = Dict.empty }
    , { loggedAt = DateTime.fromPosix (Time.millisToPosix 1234), message = Just "log line", level = LogLevel.Info, data = Dict.empty }
    ]


view : Model -> Html Msg
view _ =
    let
        entries =
            fauxEntries
                |> List.map viewEntry
    in
    div [ on "scroll" (Decode.succeed Scroll), class "log" ] entries
