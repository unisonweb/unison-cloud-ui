module UnisonCloud.Page.ServicePage.AssignedServiceDeployPage exposing (..)

import Html exposing (Html, div)
import Html.Attributes exposing (class)
import Http
import Lib.HttpApi as HttpApi
import Maybe.Extra as MaybeE
import RemoteData exposing (RemoteData(..), WebData)
import UI.ByAt as ByAt
import UI.Card as Card
import UI.ErrorCard as ErrorCard
import UI.Modal as Modal
import UI.PageContent as PageContent exposing (PageContent)
import UI.Placeholder as Placeholder
import UnisonCloud.Api as CloudApi
import UnisonCloud.AppContext exposing (AppContext)
import UnisonCloud.Log as Log
import UnisonCloud.Service exposing (Service)
import UnisonCloud.Service.ServiceName exposing (ServiceName)
import UnisonCloud.ServiceDeploy as ServiceDeploy exposing (ServiceDeploySummary)
import UnisonCloud.ServiceDeploySettings as ServiceDeploySettings
import UnisonCloud.ServiceHash exposing (ServiceHash)
import Url exposing (Url)



-- MODEL


type alias Model =
    { deploy : WebData ServiceDeploySummary
    , log : Log.Model
    , serviceDeploySettings : ServiceDeploySettings.Model
    }


init : AppContext -> ServiceName -> ServiceHash -> ( Model, Cmd Msg )
init appContext _ serviceHash =
    let
        ( log, logCmd ) =
            Log.init appContext (Log.ServiceDeployContext serviceHash)
    in
    ( { deploy = Loading
      , log = log
      , serviceDeploySettings = ServiceDeploySettings.init
      }
    , Cmd.batch [ fetchServiceDeploy appContext serviceHash, Cmd.map LogMsg logCmd ]
    )



-- UPDATE


type Msg
    = FetchServiceDeployFinished (WebData ServiceDeploySummary)
    | ServiceDeploySettingsMsg ServiceDeploySettings.Msg
    | LogMsg Log.Msg


update : AppContext -> ServiceName -> ServiceHash -> Msg -> Model -> ( Model, Cmd Msg )
update appContext _ serviceHash msg model =
    case msg of
        FetchServiceDeployFinished deploy ->
            ( { model | deploy = deploy }, Cmd.none )

        ServiceDeploySettingsMsg spMsg ->
            let
                ( serviceDeploySettings, cmd, _ ) =
                    ServiceDeploySettings.update
                        appContext
                        spMsg
                        model.serviceDeploySettings
            in
            ( { model | serviceDeploySettings = serviceDeploySettings }
            , Cmd.map ServiceDeploySettingsMsg cmd
            )

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
fetchServiceDeploy appContext serviceHash =
    CloudApi.serviceDeploy serviceHash
        |> HttpApi.toRequest
            ServiceDeploy.decodeSummary
            (RemoteData.fromResult >> FetchServiceDeployFinished)
        |> HttpApi.perform appContext.api



-- VIEW


viewLoading : Html msg
viewLoading =
    Log.viewLoading


viewError : Http.Error -> Html msg
viewError _ =
    ErrorCard.errorCard
        "Couldn't load deploy"
        "Something unexpected happened on our end when loading the deploy."
        |> ErrorCard.toCard
        |> Card.view


viewDescription : List (Html msg) -> Html msg
viewDescription content =
    div [ class "service-description" ] content


view :
    AppContext
    -> Service
    -> ServiceHash
    -> Model
    ->
        { content : PageContent Msg
        , description : Html Msg
        , modal : Maybe (Modal.Modal Msg)
        , exposedUrl : Maybe Url
        }
view appContext _ serviceHash model =
    let
        loading_ =
            { content = PageContent.oneColumn [ viewLoading ]
            , description = viewDescription [ Placeholder.view Placeholder.text ]
            , modal = Nothing
            , exposedUrl = Nothing
            }
    in
    case model.deploy of
        NotAsked ->
            loading_

        Loading ->
            loading_

        Success deploy ->
            let
                ( log, logModal ) =
                    Log.view appContext model.log

                byAt =
                    ByAt.byAt deploy.deployedBy deploy.deployedAt

                modal =
                    logModal
                        |> Maybe.map (Modal.map LogMsg)
                        |> MaybeE.orElse
                            (model.serviceDeploySettings
                                |> ServiceDeploySettings.viewModal
                                |> Maybe.map (Modal.map ServiceDeploySettingsMsg)
                            )
            in
            { content = PageContent.oneColumn [ Html.map LogMsg log ]
            , description =
                viewDescription
                    [ ByAt.view appContext.timeZone appContext.now byAt
                    , div [ class "tab-settings" ]
                        [ Html.map ServiceDeploySettingsMsg
                            (ServiceDeploySettings.viewMenu { isAssignable = False } serviceHash model.serviceDeploySettings)
                        ]
                    ]
            , modal = modal
            , exposedUrl = ServiceDeploy.exposedUrl appContext deploy
            }

        Failure e ->
            { content = PageContent.oneColumn [ viewError e ]
            , description = viewDescription []
            , modal = Nothing
            , exposedUrl = Nothing
            }
