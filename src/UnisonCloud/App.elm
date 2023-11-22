module UnisonCloud.App exposing (..)

import Browser
import Browser.Navigation as Nav
import Time
import UI.DateTime as DateTime
import UnisonCloud.AppContext exposing (AppContext)
import UnisonCloud.AppDocument as AppDocument
import UnisonCloud.AppError exposing (AppError)
import UnisonCloud.AppHeader as AppHeader
import UnisonCloud.Page.ErrorPage as ErrorPage
import UnisonCloud.Page.NotFoundPage as NotFoundPage
import UnisonCloud.Page.OverviewPage as OverviewPage
import UnisonCloud.Page.ServiceDeployPage as ServiceDeployPage
import UnisonCloud.Page.ServicePage as ServicePage
import UnisonCloud.Page.ServicesPage as ServicesPage
import UnisonCloud.Route as Route exposing (Route)
import UnisonCloud.Service.ServiceName exposing (ServiceName)
import UnisonCloud.ServiceHash exposing (ServiceHash)
import UnisonCloud.SupportChatWidget as SupportChatWidget
import Url exposing (Url)



-- MODEL


type Page
    = Overview
    | Services ServicesPage.Model
    | Service ServiceName ServicePage.Model
    | ServiceDeploy ServiceHash ServiceDeployPage.Model
    | Error AppError
    | NotFound


type AppModal
    = NoModal


type alias Model =
    { page : Page
    , appContext : AppContext
    , openedAppHeaderMenu : AppHeader.OpenedAppHeaderMenu
    , appModal : AppModal
    }


init : AppContext -> Route -> ( Model, Cmd Msg )
init appContext route =
    let
        ( page, cmd ) =
            case route of
                Route.Overview ->
                    ( Overview, Cmd.none )

                Route.Services ->
                    let
                        ( services, servicesCmd ) =
                            ServicesPage.init appContext
                    in
                    ( Services services, Cmd.map ServicesPageMsg servicesCmd )

                Route.Service id serviceRoute ->
                    let
                        ( service, serviceCmd ) =
                            ServicePage.init appContext id serviceRoute
                    in
                    ( Service id service, Cmd.map ServicePageMsg serviceCmd )

                Route.ServiceDeploy sh ->
                    let
                        ( serviceDeploy, serviceDeployCmd ) =
                            ServiceDeployPage.init appContext sh
                    in
                    ( ServiceDeploy sh serviceDeploy, Cmd.map ServiceDeployPageMsg serviceDeployCmd )

                Route.Error e ->
                    ( Error e, Cmd.none )

                Route.NotFound _ ->
                    ( NotFound, Cmd.none )

        model =
            { page = page
            , appContext = appContext
            , openedAppHeaderMenu = AppHeader.NoneOpened
            , appModal = NoModal
            }
    in
    ( model, cmd )



-- UPDATE


type Msg
    = NoOp
    | Tick Time.Posix
    | LinkClicked Browser.UrlRequest
    | UrlChanged Url
    | ToggleHelpAndResourcesMenu
    | ToggleAccountMenu
    | ServiceDeployPageMsg ServiceDeployPage.Msg
    | ServicesPageMsg ServicesPage.Msg
    | ServicePageMsg ServicePage.Msg


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case ( model.page, msg ) of
        ( _, Tick t ) ->
            let
                appContext =
                    model.appContext

                appContext_ =
                    { appContext | now = DateTime.fromPosix t }
            in
            ( { model | appContext = appContext_ }, Cmd.none )

        ( _, LinkClicked urlRequest ) ->
            case urlRequest of
                Browser.Internal url ->
                    ( model, Nav.pushUrl model.appContext.navKey (Url.toString url) )

                -- External links are handled via target blank and never end up
                -- here except for login and logout
                Browser.External url ->
                    if String.contains "logout" url || String.contains "login" url then
                        ( model, Nav.load url )

                    else
                        ( model, Cmd.none )

        ( _, UrlChanged url ) ->
            let
                route =
                    Route.fromUrl model.appContext.basePath url

                ( m, c ) =
                    case route of
                        Route.Overview ->
                            ( { model | page = Overview }, Cmd.none )

                        Route.Services ->
                            let
                                ( services, servicesCmd ) =
                                    ServicesPage.init model.appContext
                            in
                            ( { model | page = Services services }, Cmd.map ServicesPageMsg servicesCmd )

                        Route.Service serviceName serviceRoute ->
                            let
                                ( service, serviceCmd ) =
                                    ServicePage.init model.appContext serviceName serviceRoute
                            in
                            ( { model | page = Service serviceName service }, Cmd.map ServicePageMsg serviceCmd )

                        Route.ServiceDeploy serviceHash ->
                            let
                                ( serviceDeploy, serviceDeployCmd ) =
                                    ServiceDeployPage.init model.appContext serviceHash
                            in
                            ( { model | page = ServiceDeploy serviceHash serviceDeploy }, Cmd.map ServiceDeployPageMsg serviceDeployCmd )

                        Route.Error e ->
                            ( { model | page = Error e }, Cmd.none )

                        Route.NotFound _ ->
                            ( { model | page = NotFound }, Cmd.none )
            in
            ( m, c )

        ( _, ToggleHelpAndResourcesMenu ) ->
            let
                openedAppHeaderMenu =
                    if model.openedAppHeaderMenu == AppHeader.HelpAndResourcesMenu then
                        AppHeader.NoneOpened

                    else
                        AppHeader.HelpAndResourcesMenu
            in
            ( { model | openedAppHeaderMenu = openedAppHeaderMenu }, Cmd.none )

        ( _, ToggleAccountMenu ) ->
            let
                openedAppHeaderMenu =
                    if model.openedAppHeaderMenu == AppHeader.AccountMenu then
                        AppHeader.NoneOpened

                    else
                        AppHeader.AccountMenu
            in
            ( { model | openedAppHeaderMenu = openedAppHeaderMenu }, Cmd.none )

        ( Services services, ServicesPageMsg spMsg ) ->
            let
                ( services_, servicesCmd ) =
                    ServicesPage.update model.appContext spMsg services
            in
            ( { model | page = Services services_ }, Cmd.map ServicesPageMsg servicesCmd )

        ( Service serviceName service, ServicePageMsg spMsg ) ->
            let
                ( service_, serviceCmd ) =
                    ServicePage.update model.appContext serviceName spMsg service
            in
            ( { model | page = Service serviceName service_ }, Cmd.map ServicePageMsg serviceCmd )

        ( ServiceDeploy sh serviceDeploy, ServiceDeployPageMsg spMsg ) ->
            let
                ( serviceDeploy_, serviceDeployCmd ) =
                    ServiceDeployPage.update model.appContext sh spMsg serviceDeploy
            in
            ( { model | page = ServiceDeploy sh serviceDeploy_ }, Cmd.map ServiceDeployPageMsg serviceDeployCmd )

        _ ->
            ( model, Cmd.none )



-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions _ =
    Time.every 1000 Tick



-- VIEW


view : Model -> Browser.Document Msg
view model =
    let
        appContext =
            model.appContext

        appHeaderContext =
            { session = model.appContext.session
            , openedAppHeaderMenu = model.openedAppHeaderMenu
            , toggleHelpAndResourcesMenuMsg = ToggleHelpAndResourcesMenu
            , toggleAccountMenuMsg = ToggleAccountMenu
            }

        appDocument =
            case model.page of
                Overview ->
                    OverviewPage.view

                Services services ->
                    AppDocument.map
                        ServicesPageMsg
                        (ServicesPage.view appContext services)

                Service serviceName service ->
                    AppDocument.map
                        ServicePageMsg
                        (ServicePage.view appContext serviceName service)

                ServiceDeploy serviceHash service ->
                    AppDocument.map
                        ServiceDeployPageMsg
                        (ServiceDeployPage.view appContext serviceHash service)

                Error err ->
                    ErrorPage.view err

                NotFound ->
                    NotFoundPage.view
    in
    AppDocument.view
        appHeaderContext
        appDocument
        [ SupportChatWidget.view model.appContext.session ]
