module UnisonCloud.Page.ServiceDeployPage exposing (..)

import Html
import UI.AppDocument exposing (AppDocument)
import UI.PageContent as PageContent
import UI.PageLayout as PageLayout
import UI.PageTitle as PageTitle
import UnisonCloud.AppHeader as Appheader
import UnisonCloud.Env exposing (Env)
import UnisonCloud.Log as Log
import UnisonCloud.ServiceHash as ServiceHash exposing (ServiceHash)


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


view : Env -> ServiceHash -> Model -> AppDocument Msg
view env sh model =
    let
        log =
            Log.view env model.log

        page =
            PageLayout.centeredLayout
                (PageContent.oneColumn [ Html.map LogMsg log ]
                    |> PageContent.withPageTitle (PageTitle.title ("Service: " ++ ServiceHash.toString sh))
                )
                (PageLayout.PageFooter [])
                |> PageLayout.withSubduedBackground
    in
    { pageId = "service-page"
    , title = "Service " ++ ServiceHash.toString sh ++ " | Unison Cloud"
    , announcement = Nothing
    , appHeader = Appheader.appHeader
    , pageHeader = Nothing
    , page = PageLayout.view page
    , modal = Nothing
    }
