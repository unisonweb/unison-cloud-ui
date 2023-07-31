module UnisonCloud.App exposing (..)

import Browser
import Browser.Navigation as Nav
import UI.AppDocument as AppDocument
import UnisonCloud.Env exposing (Env)
import UnisonCloud.Page.NotFoundPage as NotFoundPage
import UnisonCloud.Page.OverviewPage as OverviewPage
import UnisonCloud.Page.ServicePage as ServicePage
import UnisonCloud.Page.ServicesPage as ServicesPage
import UnisonCloud.Route as Route exposing (Route)
import UnisonCloud.ServiceHash exposing (ServiceHash)
import Url exposing (Url)



-- MODEL


type Page
    = Overview
    | Services ServicesPage.Model
    | Service ServiceHash ServicePage.Model
    | NotFound


type AppModal
    = NoModal


type alias Model =
    { page : Page
    , env : Env
    , appModal : AppModal
    }


init : Env -> Route -> ( Model, Cmd Msg )
init env route =
    let
        ( page, cmd ) =
            case route of
                Route.Overview ->
                    ( Overview, Cmd.none )

                Route.Services ->
                    let
                        ( services, servicesCmd ) =
                            ServicesPage.init env
                    in
                    ( Services services, Cmd.map ServicesPageMsg servicesCmd )

                Route.Service sh ->
                    let
                        ( service, serviceCmd ) =
                            ServicePage.init env sh
                    in
                    ( Service sh service, Cmd.map ServicePageMsg serviceCmd )

                Route.NotFound _ ->
                    ( NotFound, Cmd.none )

        model =
            { page = page, env = env, appModal = NoModal }
    in
    ( model, cmd )



-- UPDATE


type Msg
    = NoOp
    | LinkClicked Browser.UrlRequest
    | UrlChanged Url
    | ServicePageMsg ServicePage.Msg
    | ServicesPageMsg ServicesPage.Msg


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case ( model.page, msg ) of
        ( _, LinkClicked urlRequest ) ->
            case urlRequest of
                Browser.Internal url ->
                    ( model, Nav.pushUrl model.env.navKey (Url.toString url) )

                -- External links are handled via target blank and never end up
                -- here
                Browser.External _ ->
                    ( model, Cmd.none )

        ( _, UrlChanged url ) ->
            let
                route =
                    Route.fromUrl model.env.basePath url

                ( m, c ) =
                    case route of
                        Route.Overview ->
                            ( { model | page = Overview }, Cmd.none )

                        Route.Services ->
                            let
                                ( services, servicesCmd ) =
                                    ServicesPage.init model.env
                            in
                            ( { model | page = Services services }, Cmd.map ServicesPageMsg servicesCmd )

                        Route.Service serviceHash ->
                            let
                                ( service, serviceCmd ) =
                                    ServicePage.init model.env serviceHash
                            in
                            ( { model | page = Service serviceHash service }, Cmd.map ServicePageMsg serviceCmd )

                        Route.NotFound _ ->
                            ( { model | page = NotFound }, Cmd.none )
            in
            ( m, c )

        ( Services services, ServicesPageMsg spMsg ) ->
            let
                ( services_, servicesCmd ) =
                    ServicesPage.update model.env spMsg services
            in
            ( { model | page = Services services_ }, Cmd.map ServicesPageMsg servicesCmd )

        ( Service sh service, ServicePageMsg spMsg ) ->
            let
                ( service_, serviceCmd ) =
                    ServicePage.update model.env sh spMsg service
            in
            ( { model | page = Service sh service_ }, Cmd.map ServicePageMsg serviceCmd )

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

                Service serviceHash service ->
                    AppDocument.map
                        ServicePageMsg
                        (ServicePage.view model.env serviceHash service)

                NotFound ->
                    NotFoundPage.view
    in
    AppDocument.view appDocument
