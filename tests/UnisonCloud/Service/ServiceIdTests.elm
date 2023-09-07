module UnisonCloud.Service.ServiceIdTests exposing (..)

import Expect
import Test exposing (..)
import UnisonCloud.Service.ServiceId as ServiceId exposing (ServiceId)


fromString : Test
fromString =
    describe "ServiceHash.fromString"
        [ test "parsing a valid string should succeed" <|
            \_ ->
                Expect.equal
                    ("e08984f8-c135-4032-8e86-6c481e0198e4"
                        |> ServiceId.fromString
                        |> Maybe.map ServiceId.toString
                        |> Maybe.withDefault "FAIL!"
                    )
                    "e08984f8-c135-4032-8e86-6c481e0198e4"
        , test "parsing a string with space should fail" <|
            \_ ->
                Expect.equal
                    ("1235432 asdasf"
                        |> ServiceId.fromString
                    )
                    Nothing
        , test "parsing a string with symbols should fail" <|
            \_ ->
                Expect.equal
                    ("asdasd4351dsadsa288##!@"
                        |> ServiceId.fromString
                    )
                    Nothing
        ]
