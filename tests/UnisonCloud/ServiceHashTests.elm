module UnisonCloud.ServiceHashTests exposing (..)

import Expect
import Test exposing (..)
import UnisonCloud.ServiceHash as ServiceHash exposing (ServiceHash)


fromUrlString : Test
fromUrlString =
    describe "ServiceHash.fromUrlString"
        [ test "parse a URL string into a ServiceHash" <|
            \_ ->
                Expect.equal
                    ("asdf"
                        |> ServiceHash.fromUrlString
                        |> Maybe.map ServiceHash.toString
                        |> Maybe.withDefault "FAIL!"
                    )
                    (ServiceHash.toString testHash)
        ]


fromApiString : Test
fromApiString =
    describe "ServiceHash.fromApiString"
        [ test "parse an API string into a ServiceHash" <|
            \_ ->
                Expect.equal
                    ("asdf"
                        |> ServiceHash.fromUrlString
                        |> Maybe.map ServiceHash.toString
                        |> Maybe.withDefault "FAIL!"
                    )
                    (ServiceHash.toString testHash)
        ]


fromString : Test
fromString =
    describe "ServiceHash.fromString"
        [ test "parse a # prefixed string into a ServiceHash" <|
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
        [ test "render the hash as a string with a prefix" <|
            \_ ->
                Expect.equal "#asdf" (ServiceHash.toString testHash)
        ]


toUrlString : Test
toUrlString =
    describe "ServiceHash.toUrlString"
        [ test "render the hash as a string without a prefix" <|
            \_ ->
                Expect.equal "asdf" (ServiceHash.toUrlString testHash)
        ]


toApiString : Test
toApiString =
    describe "ServiceHash.toApiString"
        [ test "render the hash as a string without a prefix" <|
            \_ ->
                Expect.equal "asdf" (ServiceHash.toUrlString testHash)
        ]


testHash : ServiceHash
testHash =
    ServiceHash.unsafeFromString "asdf"
