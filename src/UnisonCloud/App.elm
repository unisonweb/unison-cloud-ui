module UnisonCloud.App exposing (..)

import Browser
import Browser.Navigation as Nav
import UI.AppDocument as AppDocument
import UnisonCloud.AppContext exposing (AppContext)
import UnisonCloud.AppError exposing (AppError)
import UnisonCloud.Page.ErrorPage as ErrorPage
import UnisonCloud.Page.NotFoundPage as NotFoundPage
import UnisonCloud.Page.OverviewPage as OverviewPage
import UnisonCloud.Page.ServiceDeployPage as ServiceDeployPage
import UnisonCloud.Page.ServicePage as ServicePage
import UnisonCloud.Page.ServicesPage as ServicesPage
import UnisonCloud.Route as Route exposing (Route)
import UnisonCloud.Service.ServiceId exposing (ServiceId)
import UnisonCloud.ServiceHash exposing (ServiceHash)
import Url exposing (Url)



-- MODEL


type Page
    = Overview
    | Services ServicesPage.Model
    | Service ServiceId ServicePage.Model
    | ServiceDeploy ServiceHash ServiceDeployPage.Model
    | Error AppError
    | NotFound


type AppModal
    = NoModal


type alias Model =
    { page : Page
    , appContext : AppContext
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
            { page = page, appContext = appContext, appModal = NoModal }
    in
    ( model, cmd )



-- UPDATE


type Msg
    = NoOp
    | LinkClicked Browser.UrlRequest
    | UrlChanged Url
    | ServiceDeployPageMsg ServiceDeployPage.Msg
    | ServicesPageMsg ServicesPage.Msg
    | ServicePageMsg ServicePage.Msg


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case ( model.page, msg ) of
        ( _, LinkClicked urlRequest ) ->
            case urlRequest of
                Browser.Internal url ->
                    ( model, Nav.pushUrl model.appContext.navKey (Url.toString url) )

                -- External links are handled via target blank and never end up
                -- here
                Browser.External _ ->
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

                        Route.Service serviceId serviceRoute ->
                            let
                                ( service, serviceCmd ) =
                                    ServicePage.init model.appContext serviceId serviceRoute
                            in
                            ( { model | page = Service serviceId service }, Cmd.map ServicePageMsg serviceCmd )

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

        ( Services services, ServicesPageMsg spMsg ) ->
            let
                ( services_, servicesCmd ) =
                    ServicesPage.update model.appContext spMsg services
            in
            ( { model | page = Services services_ }, Cmd.map ServicesPageMsg servicesCmd )

        ( Service serviceId service, ServicePageMsg spMsg ) ->
            let
                ( service_, serviceCmd ) =
                    ServicePage.update model.appContext serviceId spMsg service
            in
            ( { model | page = Service serviceId service_ }, Cmd.map ServicePageMsg serviceCmd )

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
    Sub.none



-- VIEW


view : Model -> Browser.Document Msg
view model =
    let
        appContext =
            model.appContext

        appDocument =
            case model.page of
                Overview ->
                    OverviewPage.view

                Services services ->
                    AppDocument.map
                        ServicesPageMsg
                        (ServicesPage.view appContext services)

                Service serviceId service ->
                    AppDocument.map
                        ServicePageMsg
                        (ServicePage.view appContext serviceId service)

                ServiceDeploy serviceHash service ->
                    AppDocument.map
                        ServiceDeployPageMsg
                        (ServiceDeployPage.view appContext serviceHash service)

                Error err ->
                    ErrorPage.view err

                NotFound ->
                    NotFoundPage.view
    in
    AppDocument.view appDocument
