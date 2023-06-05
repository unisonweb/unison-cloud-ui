module UnisonCloud.PreApp exposing (..)

import Browser
import Browser.Navigation as Nav
import Html exposing (Html, div, p, text)
import Html.Attributes exposing (class, id, title)
import Http
import Lib.HttpApi as HttpApi exposing (HttpResult)
import Lib.Util as Util
import UI.Icon as Icon
import UI.PageContent as PageContent
import UI.PageLayout as PageLayout
import UnisonCloud.Api as CloudApi
import UnisonCloud.App as App
import UnisonCloud.AppHeader as AppHeader
import UnisonCloud.Env as Env exposing (Flags)
import UnisonCloud.PageFooter as PageFooter
import UnisonCloud.Route as Route exposing (Route)
import UnisonCloud.Session as Session exposing (Session)
import Url exposing (Url)


type Model
    = Initializing PreEnv
    | InitializationError PreEnv Http.Error
    | Initialized App.Model


type alias PreEnv =
    { flags : Flags
    , route : Route
    , navKey : Nav.Key
    }


init : Flags -> Url -> Nav.Key -> ( Model, Cmd Msg )
init flags url navKey =
    let
        route =
            Route.fromUrl flags.basePath url

        preEnv =
            { flags = flags
            , route = route
            , navKey = navKey
            }
    in
    ( Initializing preEnv, fetchSession preEnv )


type Msg
    = AppMsg App.Msg
    | FetchSessionFinished (HttpResult Session)


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case ( model, msg ) of
        ( Initializing preEnv, FetchSessionFinished sessionResult ) ->
            case sessionResult of
                Ok session ->
                    let
                        env =
                            Env.init preEnv.flags preEnv.navKey session

                        ( app, cmd ) =
                            App.init env preEnv.route
                    in
                    ( Initialized app, Cmd.map AppMsg cmd )

                Err e ->
                    case e of
                        Http.BadStatus 401 ->
                            let
                                env =
                                    Env.init
                                        preEnv.flags
                                        preEnv.navKey
                                        Session.Anonymous

                                ( app, cmd ) =
                                    App.init env preEnv.route
                            in
                            ( Initialized app, Cmd.map AppMsg cmd )

                        _ ->
                            ( InitializationError preEnv e, Cmd.none )

        ( _, AppMsg appMsg ) ->
            case model of
                Initialized a ->
                    let
                        ( app, cmd ) =
                            App.update appMsg a
                    in
                    ( Initialized app, Cmd.map AppMsg cmd )

                _ ->
                    ( model, Cmd.none )

        _ ->
            ( model, Cmd.none )


fetchSession : PreEnv -> Cmd Msg
fetchSession preEnv =
    let
        api =
            HttpApi.httpApi True preEnv.flags.apiUrl preEnv.flags.xsrfToken
    in
    CloudApi.session
        |> HttpApi.toRequest Session.decode FetchSessionFinished
        |> HttpApi.perform api


subscriptions : Model -> Sub Msg
subscriptions model =
    case model of
        Initialized app ->
            Sub.map AppMsg (App.subscriptions app)

        _ ->
            Sub.none


viewAppLoading : Html msg
viewAppLoading =
    div [ id "app" ]
        [ AppHeader.viewBlank
        , PageLayout.view
            (PageLayout.centeredLayout
                PageContent.empty
                PageFooter.pageFooter
            )
        ]


viewAppError : Http.Error -> Html msg
viewAppError error =
    div [ id "app" ]
        [ AppHeader.viewBlank
        , PageLayout.view
            (PageLayout.centeredLayout
                (PageContent.oneColumn
                    [ div [ class "app-error" ]
                        [ Icon.view Icon.warn
                        , p [ title (Util.httpErrorToString error) ]
                            [ text "Unison Cloud could not be started." ]
                        ]
                    ]
                )
                PageFooter.pageFooter
            )
        ]


view : Model -> Browser.Document Msg
view model =
    case model of
        Initializing _ ->
            { title = "Loading.. | Unison Cloud"
            , body = [ viewAppLoading ]
            }

        InitializationError _ error ->
            { title = "Application Error | Unison Cloud"
            , body = [ viewAppError error ]
            }

        Initialized appModel ->
            let
                app =
                    App.view appModel
            in
            { title = app.title
            , body = List.map (Html.map AppMsg) app.body
            }
