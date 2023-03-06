module UnisonCloud.App exposing (..)

import Browser
import UI.AppDocument as AppDocument
import UnisonCloud.Env exposing (Env)
import UnisonCloud.Page.OverviewPage as OverviewPage
import UnisonCloud.Route as Route exposing (Route)



-- MODEL


type Page
    = Overview
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

                Route.NotFound _ ->
                    ( NotFound, Cmd.none )

        model =
            { page = page, env = env, appModal = NoModal }
    in
    ( model, cmd )



-- UPDATE


type Msg
    = NoOp


update : Msg -> Model -> ( Model, Cmd Msg )
update _ model =
    ( model, Cmd.none )



-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions _ =
    Sub.none



-- VIEW


view : Model -> Browser.Document Msg
view _ =
    let
        appDocument =
            OverviewPage.view
    in
    AppDocument.view appDocument
