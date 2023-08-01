module UnisonCloud.ServiceHashTests exposing (..)

import Expect
import Test exposing (..)
import UnisonCloud.ServiceHash as ServiceHash exposing (ServiceHash)


fromUrlString : Test
fromUrlString =
    describe "ServiceHash.fromUrlString"
        [ test "parse a url string into a hash" <|
            \_ ->
                Expect.equal
                    ("@asdf"
                        |> ServiceHash.fromUrlString
                        |> Maybe.map ServiceHash.toString
                        |> Maybe.withDefault "FAIL!"
                    )
                    (ServiceHash.toString testHash)
        ]


fromString : Test
fromString =
    describe "ServiceHash.fromString"
        [ test "String version of the log level" <|
            \_ ->
                Expect.equal
                    ("#asdf"
                        |> ServiceHash.fromString
                        |> Maybe.map ServiceHash.toString
                        |> Maybe.withDefault "FAIL!"
                    )
                    (ServiceHash.toString testHash)
        ]


toString : Test
toString =
    describe "ServiceHash.toString"
        [ test "String version of the log level" <|
            \_ ->
                Expect.equal "#asdf" (ServiceHash.toString testHash)
        ]


toUrlString : Test
toUrlString =
    describe "ServiceHash.toUrlString"
        [ test "String version of the log level" <|
            \_ ->
                Expect.equal "@asdf" (ServiceHash.toUrlString testHash)
        ]


testHash : ServiceHash
testHash =
    ServiceHash.unsafeFromString "asdf"
