module UnisonCloud.Page.ServiceDeployPage exposing (..)

import Html
import UI.AppDocument exposing (AppDocument)
import UI.PageContent as PageContent
import UI.PageLayout as PageLayout
import UI.PageTitle as PageTitle
import UnisonCloud.AppContext exposing (AppContext)
import UnisonCloud.AppHeader as Appheader
import UnisonCloud.Log as Log
import UnisonCloud.ServiceHash as ServiceHash exposing (ServiceHash)


type alias Model =
    { log : Log.Model
    }


init : AppContext -> ServiceHash -> ( Model, Cmd Msg )
init appContext sh =
    let
        ( log, logCmd ) =
            Log.init appContext (Log.ServiceDeployContext sh)
    in
    ( { log = log }, Cmd.map LogMsg logCmd )


type Msg
    = LogMsg Log.Msg


update : AppContext -> ServiceHash -> Msg -> Model -> ( Model, Cmd Msg )
update appContext _ msg model =
    case msg of
        LogMsg logMsg ->
            let
                ( log, logCmd ) =
                    Log.update appContext logMsg model.log
            in
            ( { model | log = log }, Cmd.map LogMsg logCmd )


view : AppContext -> ServiceHash -> Model -> AppDocument Msg
view appContext sh model =
    let
        log =
            Log.view appContext model.log

        page =
            PageLayout.centeredLayout
                (PageContent.oneColumn [ Html.map LogMsg log ]
                    |> PageContent.withPageTitle (PageTitle.title ("Service: " ++ ServiceHash.toShortString sh))
                )
                (PageLayout.PageFooter [])
                |> PageLayout.withSubduedBackground
    in
    { pageId = "service-page"
    , title = "Service Deploy: " ++ ServiceHash.toShortString sh ++ " | Unison Cloud"
    , announcement = Nothing
    , appHeader = Appheader.appHeader
    , pageHeader = Nothing
    , page = PageLayout.view page
    , modal = Nothing
    }
