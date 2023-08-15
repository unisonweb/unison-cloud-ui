module UnisonCloud.Page.ServiceDeployPage exposing (..)

import Html exposing (text)
import Lib.HttpApi as HttpApi
import RemoteData exposing (RemoteData(..), WebData)
import UI.AppDocument exposing (AppDocument)
import UI.PageContent as PageContent
import UI.PageLayout as PageLayout
import UI.PageTitle as PageTitle
import UnisonCloud.Api as CloudApi
import UnisonCloud.AppContext exposing (AppContext)
import UnisonCloud.AppHeader as Appheader
import UnisonCloud.Log as Log
import UnisonCloud.ServiceDeploy as ServiceDeploy exposing (ServiceDeploy)
import UnisonCloud.ServiceHash as ServiceHash exposing (ServiceHash)



-- MODEL


type alias Model =
    { deploy : WebData ServiceDeploy
    , log : Log.Model
    }


init : AppContext -> ServiceHash -> ( Model, Cmd Msg )
init appContext serviceHash =
    let
        ( log, logCmd ) =
            Log.init appContext (Log.ServiceDeployContext serviceHash)
    in
    ( { deploy = Loading, log = log }, Cmd.batch [ fetchServiceDeploy appContext serviceHash, Cmd.map LogMsg logCmd ] )



-- UPDATE


type Msg
    = FetchServiceDeployFinished (WebData ServiceDeploy)
    | LogMsg Log.Msg


update : AppContext -> ServiceHash -> Msg -> Model -> ( Model, Cmd Msg )
update appContext _ msg model =
    case msg of
        FetchServiceDeployFinished deploy ->
            ( { model | deploy = deploy }, Cmd.none )

        LogMsg logMsg ->
            let
                ( log, logCmd ) =
                    Log.update appContext logMsg model.log
            in
            ( { model | log = log }, Cmd.map LogMsg logCmd )



-- FFECT


fetchServiceDeploy : AppContext -> ServiceHash -> Cmd Msg
fetchServiceDeploy appContext sh =
    CloudApi.serviceDeploy sh
        |> HttpApi.toRequest
            ServiceDeploy.decode
            (RemoteData.fromResult >> FetchServiceDeployFinished)
        |> HttpApi.perform appContext.api



-- VIEW


view : AppContext -> ServiceHash -> Model -> AppDocument Msg
view appContext sh model =
    let
        content =
            case model.deploy of
                Success _ ->
                    let
                        log =
                            Log.view appContext model.log
                    in
                    [ Html.map LogMsg log ]

                _ ->
                    [ text "TODO" ]

        page =
            PageLayout.centeredLayout
                (PageContent.oneColumn content
                    |> PageContent.withPageTitle (PageTitle.title ("Service Deploy: " ++ ServiceHash.toShortString sh))
                )
                (PageLayout.PageFooter [])
                |> PageLayout.withSubduedBackground
    in
    { pageId = "service-deploy-page"
    , title = "Service Deploy: " ++ ServiceHash.toShortString sh ++ " | Unison Cloud"
    , announcement = Nothing
    , appHeader = Appheader.appHeader
    , pageHeader = Nothing
    , page = PageLayout.view page
    , modal = Nothing
    }
