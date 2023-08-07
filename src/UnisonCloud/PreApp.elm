module UnisonCloud.PreApp exposing (..)

import Browser
import Browser.Navigation as Nav
import Html exposing (Html, div, p, text)
import Html.Attributes exposing (class, id)
import Http
import Lib.HttpApi as HttpApi exposing (HttpResult)
import Task exposing (Task)
import Time
import UI.Button as Button
import UI.DateTime as DateTime
import UI.Icon as Icon
import UI.PageContent as PageContent
import UI.PageLayout as PageLayout
import UnisonCloud.Api as CloudApi
import UnisonCloud.App as App
import UnisonCloud.AppHeader as AppHeader
import UnisonCloud.Env as Env exposing (Flags)
import UnisonCloud.Link as Link
import UnisonCloud.PageFooter as PageFooter
import UnisonCloud.Route as Route exposing (Route)
import UnisonCloud.Session as Session exposing (Session)
import Url exposing (Url)


type AppError
    = NetworkError Http.Error


type Model
    = Initializing PreEnv
    | InitializationError PreEnv AppError
    | NotSignedIn PreEnv
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
    ( Initializing preEnv, Task.attempt FetchTimeAndZoneFinished (fetchTimeAndZone preEnv) )


type Msg
    = AppMsg App.Msg
    | FetchTimeAndZoneFinished (HttpResult ( Time.Posix, Time.Zone, Session ))


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case ( model, msg ) of
        ( Initializing preEnv, FetchTimeAndZoneFinished (Ok ( now, timeZone, session )) ) ->
            let
                env =
                    Env.init preEnv.flags
                        preEnv.navKey
                        (DateTime.fromPosix now)
                        timeZone
                        session

                ( app, cmd ) =
                    App.init env preEnv.route
            in
            ( Initialized app, Cmd.map AppMsg cmd )

        ( Initializing preEnv, FetchTimeAndZoneFinished (Err e) ) ->
            case e of
                Http.BadStatus 401 ->
                    ( NotSignedIn preEnv, Cmd.none )

                _ ->
                    ( InitializationError preEnv (NetworkError e), Cmd.none )

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



-- EFFECTS


fetchTimeAndZone : PreEnv -> Task Http.Error ( Time.Posix, Time.Zone, Session )
fetchTimeAndZone preEnv =
    Task.map3 (\n z s -> ( n, z, s )) Time.now Time.here (fetchSession preEnv)


fetchSession : PreEnv -> Task Http.Error Session
fetchSession preEnv =
    let
        apiUrl =
            HttpApi.apiUrlFromString True preEnv.flags.apiUrl
    in
    HttpApi.toTask apiUrl Session.decode CloudApi.session


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


viewAppError : AppError -> Html msg
viewAppError _ =
    div [ id "app" ]
        [ AppHeader.viewBlank
        , PageLayout.view
            (PageLayout.centeredLayout
                (PageContent.oneColumn
                    [ div [ class "app-error" ]
                        [ Icon.view Icon.warn
                        , p [] [ text "Unison Cloud could not be started." ]
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

        NotSignedIn _ ->
            { title = "Unison Cloud"
            , body =
                [ div [ id "app", class "sign-in-page" ]
                    [ AppHeader.viewBlank
                    , PageLayout.view
                        (PageLayout.centeredLayout
                            (PageContent.oneColumn
                                [ Button.button_ Link.login "Sign In to Unison Cloud"
                                    |> Button.medium
                                    |> Button.decorativeBlue
                                    |> Button.view
                                ]
                            )
                            PageFooter.pageFooter
                        )
                    ]
                ]
            }

        Initialized appModel ->
            let
                app =
                    App.view appModel
            in
            { title = app.title
            , body = List.map (Html.map AppMsg) app.body
            }
