module UnisonCloud.Service.ServiceNameTests exposing (..)

import Expect
import Test exposing (..)
import UnisonCloud.Service.ServiceName as ServiceName


fromString : Test
fromString =
    describe "ServiceHash.fromString"
        [ test "parsing a valid string should succeed" <|
            \_ ->
                Expect.equal
                    ("production-chatbot"
                        |> ServiceName.fromString
                        |> Maybe.map ServiceName.toString
                        |> Maybe.withDefault "FAIL!"
                    )
                    "production-chatbot"
        , test "parsing a valid string with numbers should succeed" <|
            \_ ->
                Expect.equal
                    ("production-chatbot3"
                        |> ServiceName.fromString
                        |> Maybe.map ServiceName.toString
                        |> Maybe.withDefault "FAIL!"
                    )
                    "production-chatbot3"
        , test "parsing a string with space should fail" <|
            \_ ->
                Expect.equal
                    ("production chatbot"
                        |> ServiceName.fromString
                    )
                    Nothing
        , test "parsing a string with symbols should fail" <|
            \_ ->
                Expect.equal
                    ("production_=f2chatbot"
                        |> ServiceName.fromString
                    )
                    Nothing
        ]
