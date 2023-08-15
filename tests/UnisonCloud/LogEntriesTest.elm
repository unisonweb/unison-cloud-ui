module UnisonCloud.LogEntriesTest exposing (..)

import Dict
import Expect
import Test exposing (..)
import Time
import UI.DateTime as DateTime exposing (DateTime)
import UnisonCloud.LogEntries as LogEntries
import UnisonCloud.LogLevel as LogLevel
import UnisonCloud.LogLine exposing (LogLine)


fromLines : Test
fromLines =
    describe "LogEntries.fromString"
        [ test "String version of the log level" <|
            \_ ->
                let
                    result =
                        LogEntries.fromLines Time.utc logLines
                            |> List.map logEntryToString

                    expected =
                        [ LogEntries.Line logLines_.newest
                        , LogEntries.Line logLines_.new
                        , LogEntries.Line logLines_.old
                        , LogEntries.DateBoundary logLines_.old.loggedAt
                        , LogEntries.Line logLines_.oldest
                        , LogEntries.DateBoundary logLines_.oldest.loggedAt
                        ]
                            |> List.map logEntryToString
                in
                Expect.equal expected result
        ]



-- HELPERS


logLines_ : { oldest : LogLine, old : LogLine, new : LogLine, newest : LogLine }
logLines_ =
    { oldest =
        { loggedAt = dateTimeUnsafeFromString "2023-08-14T10:00:00.998Z"
        , id = "04cc33f3-7ebb-4dc4-153d-970140e11e4e"
        , message = Nothing
        , level = LogLevel.Info
        , data = Dict.fromList [ ( "method ", "GET" ) ]
        }
    , old =
        { loggedAt = dateTimeUnsafeFromString "2023-08-15T11:00:00.998Z"
        , id = "0f006c16-741c-431d-c06a-462a850cdf36"
        , message = Nothing
        , level = LogLevel.Info
        , data = Dict.empty
        }
    , new =
        { loggedAt = dateTimeUnsafeFromString "2023-08-15T12:00:00.998Z"
        , id = "09a40f5d-0e34-4937-eb75-f8dc026ef74c"
        , message = Nothing
        , level = LogLevel.Info
        , data = Dict.empty
        }
    , newest =
        { loggedAt = dateTimeUnsafeFromString "2023-08-15T15:00:00.998Z"
        , id = "06ca47cc-736a-4aa8-9366-db668b5992fc"
        , message = Nothing
        , level = LogLevel.Info
        , data = Dict.empty
        }
    }


logLines : List LogLine
logLines =
    [ logLines_.oldest, logLines_.old, logLines_.new, logLines_.newest ]


logEntryToString : LogEntries.LogEntry -> String
logEntryToString entry =
    case entry of
        LogEntries.Line l ->
            "Log Line: " ++ DateTime.toISO8601 l.loggedAt

        LogEntries.DateBoundary d ->
            "Date Boundary: " ++ DateTime.toISO8601 d


{-| TODO: Port to UI.DateTime
-}
dateTimeUnsafeFromString : String -> DateTime
dateTimeUnsafeFromString s =
    let
        fallbackDateTime =
            DateTime.fromPosix (Time.millisToPosix 1)
    in
    s
        |> DateTime.fromISO8601
        |> Maybe.withDefault fallbackDateTime
