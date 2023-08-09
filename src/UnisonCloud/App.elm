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
import UnisonCloud.Page.ServicesPage as ServicesPage
import UnisonCloud.Route as Route exposing (Route)
import UnisonCloud.ServiceHash exposing (ServiceHash)
import Url exposing (Url)



-- MODEL


type Page
    = Overview
    | Services ServicesPage.Model
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

                Route.ServiceDeploy sh ->
                    let
                        ( service, serviceCmd ) =
                            ServiceDeployPage.init appContext sh
                    in
                    ( ServiceDeploy sh service, Cmd.map ServicePageMsg serviceCmd )

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
    | ServicePageMsg ServiceDeployPage.Msg
    | ServicesPageMsg ServicesPage.Msg


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

                        Route.ServiceDeploy serviceHash ->
                            let
                                ( service, serviceCmd ) =
                                    ServiceDeployPage.init model.appContext serviceHash
                            in
                            ( { model | page = ServiceDeploy serviceHash service }, Cmd.map ServicePageMsg serviceCmd )

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

        ( ServiceDeploy sh service, ServicePageMsg spMsg ) ->
            let
                ( service_, serviceCmd ) =
                    ServiceDeployPage.update model.appContext sh spMsg service
            in
            ( { model | page = ServiceDeploy sh service_ }, Cmd.map ServicePageMsg serviceCmd )

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
        appDocument =
            case model.page of
                Overview ->
                    OverviewPage.view

                Services services ->
                    ServicesPage.view services

                ServiceDeploy serviceHash service ->
                    AppDocument.map
                        ServicePageMsg
                        (ServiceDeployPage.view model.appContext serviceHash service)

                Error err ->
                    ErrorPage.view err

                NotFound ->
                    NotFoundPage.view
    in
    AppDocument.view appDocument
