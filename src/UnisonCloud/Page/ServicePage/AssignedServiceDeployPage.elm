module UnisonCloud.Page.ServicePage.AssignedServiceDeployPage exposing (..)

import Html exposing (Html, div)
import Html.Attributes exposing (class)
import Http
import Lib.HttpApi as HttpApi
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
import UnisonCloud.Service.ServiceId exposing (ServiceId)
import UnisonCloud.ServiceDeploy as ServiceDeploy exposing (ServiceDeploySummary)
import UnisonCloud.ServiceHash exposing (ServiceHash)



-- MODEL


type alias Model =
    { deploy : WebData ServiceDeploySummary
    , log : Log.Model
    }


init : AppContext -> ServiceId -> ServiceHash -> ( Model, Cmd Msg )
init appContext _ serviceHash =
    let
        ( log, logCmd ) =
            Log.init appContext (Log.ServiceDeployContext serviceHash)
    in
    ( { deploy = Loading, log = log }
    , Cmd.batch [ fetchServiceDeploy appContext serviceHash, Cmd.map LogMsg logCmd ]
    )



-- UPDATE


type Msg
    = FetchServiceDeployFinished (WebData ServiceDeploySummary)
    | LogMsg Log.Msg


update : AppContext -> ServiceId -> ServiceHash -> Msg -> Model -> ( Model, Cmd Msg )
update appContext _ serviceHash msg model =
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
    div [ class "service_description" ] content


view :
    AppContext
    -> ServiceId
    -> ServiceHash
    -> Model
    -> ( PageContent Msg, Html msg, Maybe (Modal.Modal Msg) )
view appContext _ _ model =
    let
        loading_ =
            ( viewLoading
            , viewDescription [ Placeholder.view Placeholder.text ]
            , Nothing
            )

        ( content, description, modal ) =
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
                    in
                    ( Html.map LogMsg log
                    , viewDescription
                        [ ByAt.view appContext.timeZone appContext.now byAt
                        ]
                    , Maybe.map (Modal.map LogMsg) logModal
                    )

                Failure e ->
                    ( viewError e, viewDescription [], Nothing )
    in
    ( PageContent.oneColumn [ content ], description, modal )
