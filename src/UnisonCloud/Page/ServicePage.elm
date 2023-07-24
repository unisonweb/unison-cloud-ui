module UnisonCloud.Page.ServicePage exposing (..)

import Html exposing (text)
import UI.AppDocument exposing (AppDocument)
import UI.PageContent as PageContent
import UI.PageLayout as PageLayout
import UnisonCloud.AppHeader as Appheader
import UnisonCloud.Env exposing (Env)
import UnisonCloud.Log as Log
import UnisonCloud.ServiceHash exposing (ServiceHash)


type alias Model =
    { log : Log.Model
    }


init : Env -> ServiceHash -> ( Model, Cmd Msg )
init env _ =
    let
        ( log, logCmd ) =
            Log.init env
    in
    ( { log = log }, Cmd.map LogMsg logCmd )


type Msg
    = LogMsg Log.Msg


update : Env -> ServiceHash -> Msg -> Model -> ( Model, Cmd Msg )
update env _ msg model =
    case msg of
        LogMsg logMsg ->
            let
                ( log, logCmd ) =
                    Log.update env logMsg model.log
            in
            ( { model | log = log }, Cmd.map LogMsg logCmd )


view : ServiceHash -> Model -> AppDocument Msg
view _ model =
    let
        log =
            Log.view model.log

        page =
            PageLayout.centeredLayout
                (PageContent.oneColumn [ text "service page", Html.map LogMsg log ])
                (PageLayout.PageFooter [])
    in
    { pageId = "service-page"
    , title = "Unison Cloud | Service"
    , announcement = Nothing
    , appHeader = Appheader.appHeader
    , pageHeader = Nothing
    , page = PageLayout.view page
    , modal = Nothing
    }
