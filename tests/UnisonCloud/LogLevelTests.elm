module UnisonCloud.LogLevelTests exposing (..)

import Expect
import Test exposing (..)
import UnisonCloud.LogLevel as LogLevel exposing (LogLevel(..))


toString : Test
toString =
    describe "LogLevel.toString"
        [ test "String version of the log level" <|
            \_ ->
                Expect.equal
                    (List.map LogLevel.toString [ Info, Warn, Error, Custom "asdf" ])
                    [ "Info", "Warn", "Error", "asdf" ]
        ]


toClassName : Test
toClassName =
    describe "LogLevel.toClassName"
        [ test "className version of the log level" <|
            \_ ->
                Expect.equal
                    (List.map LogLevel.toClassName [ Info, Warn, Error, Custom "asdf" ])
                    [ "log-level_info", "log-level_warn", "log-level_error", "log-level_custom" ]
        ]


fromString : Test
fromString =
    describe "LogLevel.fromString"
        [ test "Parses various level formats into a unified set of levels" <|
            \_ ->
                let
                    input =
                        [ "info", "log", "warn", "warning", "error", "err", "fail", "failure", "bug", "fatal" ]

                    output =
                        List.map (LogLevel.fromString >> LogLevel.toString) input

                    expected =
                        [ Info, Info, Warn, Warn, Error, Error, Error, Error, Error, Error ]
                in
                Expect.equal output (List.map LogLevel.toString expected)
        , test "Parses unknown levels as Custom" <|
            \_ ->
                let
                    input =
                        [ "Timing", "Benchmark", "asdf1234" ]

                    expected =
                        [ Custom "Timing", Custom "Benchmark", Custom "asdf1234" ]
                in
                Expect.equal input (List.map LogLevel.toString expected)
        ]
