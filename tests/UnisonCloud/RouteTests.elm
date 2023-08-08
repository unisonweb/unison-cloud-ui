module UnisonCloud.RouteTests exposing (..)

import Expect
import Test exposing (..)
import UnisonCloud.Route as Route exposing (Route(..))
import UnisonCloud.ServiceHash as ServiceHash
import Url exposing (Url)


servicesRoute : Test
servicesRoute =
    describe "Route.fromUrl : services route"
        [ test "Matches / to Services" <|
            \_ ->
                let
                    url =
                        mkUrl "/"
                in
                Expect.equal Services (Route.fromUrl "" url)
        , test "Matches /services to Services" <|
            \_ ->
                let
                    url =
                        mkUrl "/services"
                in
                Expect.equal Services (Route.fromUrl "" url)
        ]



-- SERVICE DEPLOYS ROUTE


serviceDeployRoute : Test
serviceDeployRoute =
    describe "Route.fromUrl : service deploys route"
        [ test "Matches /service-deploys/:service-hash to ServiceDeploy " <|
            \_ ->
                let
                    url =
                        mkUrl "/service-deploys/@abc"
                in
                Expect.equal
                    (ServiceDeploy (ServiceHash.unsafeFromString "abc"))
                    (Route.fromUrl "" url)
        ]



-- HELPERS


mkUrl : String -> Url
mkUrl path =
    { protocol = Url.Https
    , host = "unison.cloud"
    , port_ = Just 443
    , path = path
    , query = Nothing
    , fragment = Nothing
    }
