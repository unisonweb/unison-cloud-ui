module UnisonCloud.LogEntriesTest exposing (..)

import Dict
import Expect
import Json.Decode as Decode
import Result.Extra as ResultE
import Test exposing (..)
import Time
import UI.DateTime as DateTime exposing (DateTime)
import UnisonCloud.LogEntries as LogEntries
import UnisonCloud.LogLevel as LogLevel
import UnisonCloud.LogLine as LogLine exposing (LogLine)


fromLines : Test
fromLines =
    describe "LogEntries.fromString"
        [ test "Inserts date boundaries" <|
            \_ ->
                let
                    result =
                        logLines
                            |> List.map (\l -> { justFetched = False, line = l })
                            |> LogEntries.fromLines Time.utc
                            |> List.map logEntryToString

                    expected =
                        [ LogEntries.Line { justFetched = False, line = logLines_.newest }
                        , LogEntries.Line { justFetched = False, line = logLines_.new }
                        , LogEntries.Line { justFetched = False, line = logLines_.old }
                        , LogEntries.DateBoundary logLines_.old.loggedAt
                        , LogEntries.Line { justFetched = False, line = logLines_.oldest }

                        -- , LogEntries.DateBoundary logLines_.oldest.loggedAt
                        ]
                            |> List.map logEntryToString
                in
                Expect.equal expected result
        , test "Inserts date boundaries (using parsed data)" <|
            \_ ->
                let
                    result =
                        logLinesRaw
                            |> List.map (\l -> { justFetched = False, line = l })
                            |> LogEntries.fromLines Time.utc
                            |> List.map logEntryToString

                    expected =
                        [ "Log Line: 2023-08-15T15:48:40.211Z"
                        , "Date Boundary: 2023-08-15T15:48:40.211Z"
                        , "Log Line: 2023-08-14T20:30:49.312Z"
                        , "Log Line: 2023-08-14T18:21:59.503Z"

                        -- , "Date Boundary: 2023-08-14T18:21:59.503Z"
                        ]
                in
                Expect.equal expected result
        ]



-- HELPERS


logLinesRaw : List LogLine
logLinesRaw =
    let
        raw =
            [ "{\"nodeId\":\"eb1d7c2208190134a78af3501c0767bf\",\"time\":\"2023-08-14T18:21:59.503091418-00:00\",\"level\":\"INFO\",\"userId\":\"5eb081899fb14b51910fb3d8e6d9dbce\",\"jobId\":\"02c8431a76a94bbd26239287379da3e8\",\"meta\":{\"serviceHash\":\"vcmaxTpA7Jpi2_xfKJo9QmSY8W7SoNb4eMqdyqgIons\"},\"envId\":\"9522b5ca-4fc9-4ecc-963b-0eb2088d3738\",\"type\":\"UserLogMsg\",\"id\":\"0acbcf81-fdf5-4761-1397-60303c25cf28\",\"userMsg\":{\"method\":\"GET\",\"level\":\"info\"}}"
            , "{\"nodeId\":\"c5fa82684abbb2e1bcaada33eb280953\",\"time\":\"2023-08-14T20:30:49.312249666-00:00\",\"level\":\"INFO\",\"userId\":\"5eb081899fb14b51910fb3d8e6d9dbce\",\"jobId\":\"06c73f8270024017d4044a004d149eea\",\"meta\":{\"serviceHash\":\"vcmaxTpA7Jpi2_xfKJo9QmSY8W7SoNb4eMqdyqgIons\"},\"envId\":\"9522b5ca-4fc9-4ecc-963b-0eb2088d3738\",\"type\":\"UserLogMsg\",\"id\":\"06ca47cc-736a-4aa8-9366-db668b5992fc\",\"userMsg\":{\"method\":\"GET\",\"level\":\"info\"}}"
            , "{\"nodeId\":\"eb1d7c2208190134a78af3501c0767bf\",\"time\":\"2023-08-15T15:48:40.211415734-00:00\",\"level\":\"INFO\",\"userId\":\"5eb081899fb14b51910fb3d8e6d9dbce\",\"jobId\":\"09bdefd0effd4c2987bbef7f32e22d1f\",\"meta\":{\"serviceHash\":\"vcmaxTpA7Jpi2_xfKJo9QmSY8W7SoNb4eMqdyqgIons\"},\"envId\":\"9522b5ca-4fc9-4ecc-963b-0eb2088d3738\",\"type\":\"UserLogMsg\",\"id\":\"06f25b2c-620d-4572-0237-8fbabb02efbd\",\"userMsg\":{\"method\":\"GET\",\"level\":\"info\"}}"
            ]
    in
    raw
        |> List.map (Decode.decodeString LogLine.decode_)
        |> ResultE.combine
        |> Result.withDefault []


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
            "Log Line: " ++ DateTime.toISO8601 l.line.loggedAt

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
