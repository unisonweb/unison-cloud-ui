module UnisonCloud.PreApp exposing (..)

import Browser
import Browser.Navigation as Nav
import Html exposing (Html, div, h1, h2, p, span, text)
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
import UnisonCloud.AppContext as AppContext exposing (Flags)
import UnisonCloud.CloudsBackground as CloudsBackground
import UnisonCloud.Link as Link
import UnisonCloud.PageFooter as PageFooter
import UnisonCloud.Route as Route exposing (Route)
import UnisonCloud.Session as Session exposing (Session)
import Url exposing (Url)


type AppError
    = NetworkError Http.Error


type Model
    = Initializing PreAppContext
    | InitializationError PreAppContext AppError
    | NotSignedIn PreAppContext
    | Initialized App.Model


type alias PreAppContext =
    { flags : Flags
    , route : Route
    , api : HttpApi.HttpApi
    , currentUrl : Url
    , navKey : Nav.Key
    }


init : Flags -> Url -> Nav.Key -> ( Model, Cmd Msg )
init flags url navKey =
    let
        route =
            Route.fromUrl flags.basePath url

        api =
            HttpApi.httpApi True flags.apiUrl flags.xsrfToken

        preAppContext =
            { flags = flags
            , route = route
            , api = api
            , currentUrl = url
            , navKey = navKey
            }
    in
    ( Initializing preAppContext
    , Task.attempt FetchPreReqsFinished (fetchPreReqs preAppContext)
    )


type Msg
    = AppMsg App.Msg
    | FetchPreReqsFinished (HttpResult ( Time.Posix, Time.Zone, Session ))


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case ( model, msg ) of
        ( Initializing preAppContext, FetchPreReqsFinished (Ok ( now, timeZone, session )) ) ->
            let
                appContext =
                    AppContext.init preAppContext.flags
                        preAppContext.navKey
                        preAppContext.currentUrl
                        (DateTime.fromPosix now)
                        timeZone
                        session

                ( app, cmd ) =
                    App.init appContext preAppContext.route
            in
            ( Initialized app, Cmd.map AppMsg cmd )

        ( Initializing preAppContext, FetchPreReqsFinished (Err e) ) ->
            case e of
                Http.BadStatus 401 ->
                    ( NotSignedIn preAppContext, Cmd.none )

                Http.BadStatus 404 ->
                    ( NotSignedIn preAppContext, Cmd.none )

                _ ->
                    ( InitializationError preAppContext (NetworkError e), Cmd.none )

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


fetchPreReqs : PreAppContext -> Task Http.Error ( Time.Posix, Time.Zone, Session )
fetchPreReqs preAppContext =
    Task.map3 (\n z s -> ( n, z, s )) Time.now Time.here (fetchSession preAppContext)


fetchSession : PreAppContext -> Task Http.Error Session
fetchSession preAppContext =
    let
        apiUrl =
            HttpApi.apiUrlFromString True preAppContext.flags.apiUrl
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
        [ PageLayout.view
            (PageLayout.centeredLayout
                PageContent.empty
                PageFooter.pageFooter
            )
        ]


viewAppError : AppError -> Html msg
viewAppError _ =
    div [ id "app" ]
        [ PageLayout.view
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
            , body =
                [ viewAppError error
                ]
            }

        NotSignedIn ctx ->
            { title = "Unison Cloud"
            , body =
                [ div [ id "app", class "sign-in-page" ]
                    [ PageLayout.view
                        (PageLayout.centeredLayout
                            (PageContent.oneColumn
                                [ div
                                    [ class "unison-cloud-box" ]
                                    [ div []
                                        [ h1 [ class "unison-cloud-wordmark" ]
                                            [ text "Unison "
                                            , span [ class "unison-cloud-wordmark_cloud" ] [ text "Cloud" ]
                                            ]
                                        , h2 [] [ text "Write code. Hit run. The cloud computes." ]
                                        , Button.iconThenLabel_
                                            (Link.login ctx.api ctx.currentUrl)
                                            Icon.cloud
                                            "Sign In"
                                            |> Button.large
                                            |> Button.emphasized
                                            |> Button.view
                                        ]
                                    , CloudsBackground.view
                                    ]
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
