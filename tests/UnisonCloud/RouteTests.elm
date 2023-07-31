module UnisonCloud.RouteTests exposing (..)

import Expect
import Test exposing (..)
import UnisonCloud.Route as Route exposing (Route(..))
import Url exposing (Url)


overviewRoute : Test
overviewRoute =
    describe "Route.fromUrl : services route"
        [ test "Matches root to Services" <|
            \_ ->
                let
                    url =
                        mkUrl "/"
                in
                Expect.equal Services (Route.fromUrl "" url)
        ]


mkUrl : String -> Url
mkUrl path =
    { protocol = Url.Https
    , host = "unison.cloud"
    , port_ = Just 443
    , path = path
    , query = Nothing
    , fragment = Nothing
    }
