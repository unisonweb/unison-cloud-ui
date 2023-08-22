module UnisonCloud.Page.ServiceDeployPage exposing (..)

import Html exposing (Html)
import Http
import Lib.HttpApi as HttpApi
import RemoteData exposing (RemoteData(..), WebData)
import UI.AppDocument exposing (AppDocument)
import UI.Card as Card
import UI.ErrorCard as ErrorCard
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
update appContext serviceHash msg model =
    case msg of
        FetchServiceDeployFinished deploy ->
            ( { model | deploy = deploy }, Cmd.none )

        LogMsg logMsg ->
            let
                ( log, logCmd ) =
                    Log.update appContext
                        (Log.ServiceDeployContext serviceHash)
                        logMsg
                        model.log
            in
            ( { model | log = log }, Cmd.map LogMsg logCmd )



-- EFFECTS


fetchServiceDeploy : AppContext -> ServiceHash -> Cmd Msg
fetchServiceDeploy appContext sh =
    CloudApi.serviceDeploy sh
        |> HttpApi.toRequest
            ServiceDeploy.decode
            (RemoteData.fromResult >> FetchServiceDeployFinished)
        |> HttpApi.perform appContext.api



-- VIEW


viewLoading : Html msg
viewLoading =
    Log.viewLoading


viewError : Http.Error -> Html msg
viewError _ =
    ErrorCard.errorCard
        "Couldn't load service deploy"
        "Something unexpected happened on our end when loading the service deploy and we can't display it."
        |> ErrorCard.toCard
        |> Card.asContainedWithFade
        |> Card.view


view : AppContext -> ServiceHash -> Model -> AppDocument Msg
view appContext sh model =
    let
        content =
            case model.deploy of
                NotAsked ->
                    [ viewLoading ]

                Loading ->
                    [ viewLoading ]

                Success _ ->
                    let
                        log =
                            Log.view appContext model.log
                    in
                    [ Html.map LogMsg log ]

                Failure e ->
                    [ viewError e ]

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
