module UnisonCloud.Page.ServiceDeployPage exposing (..)

import Html exposing (Html, div, text)
import Html.Attributes exposing (class)
import Http
import Lib.HttpApi as HttpApi
import RemoteData exposing (RemoteData(..), WebData)
import UI
import UI.ByAt as ByAt
import UI.Card as Card
import UI.Click as Click
import UI.ErrorCard as ErrorCard
import UI.ExternalLinkIcon as ExternalLinkIcon
import UI.Modal as Modal
import UI.PageContent as PageContent
import UI.PageLayout as PageLayout
import UI.PageTitle as PageTitle
import UI.Placeholder as Placeholder
import UI.TabList as TabList
import UnisonCloud.Api as CloudApi
import UnisonCloud.AppContext exposing (AppContext)
import UnisonCloud.AppDocument exposing (AppDocument)
import UnisonCloud.AppHeader as Appheader
import UnisonCloud.Link as Link
import UnisonCloud.Log as Log
import UnisonCloud.ServiceDeploy as ServiceDeploy exposing (ServiceDeploySummary)
import UnisonCloud.ServiceHash as ServiceHash exposing (ServiceHash)
import Url



-- MODEL


type alias Model =
    { deploy : WebData ServiceDeploySummary
    , log : Log.Model
    }


init : AppContext -> ServiceHash -> ( Model, Cmd Msg )
init appContext serviceHash =
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
        "Couldn't load service deploy"
        "Something unexpected happened on our end when loading the service deploy and we can't display it."
        |> ErrorCard.toCard
        |> Card.view


viewDescription : AppContext -> ServiceDeploySummary -> List (Html msg) -> Html msg
viewDescription appContext deploy content =
    let
        hash =
            ServiceHash.toShortString deploy.hash

        exposedLink =
            case ServiceDeploy.exposedUrl appContext deploy of
                Just u ->
                    ExternalLinkIcon.view (Click.externalHref (Url.toString u))

                Nothing ->
                    UI.nothing
    in
    div [ class "service-deploy_description" ]
        (div [ class "service-deploy_description_hash" ]
            [ text hash, exposedLink ]
            :: content
        )


viewSimpleDescription : ServiceHash -> List (Html msg) -> Html msg
viewSimpleDescription serviceHash content =
    let
        hash =
            ServiceHash.toShortString serviceHash
    in
    div [ class "service-deploy_description" ]
        (div [ class "service-deploy_description_hash" ]
            [ text hash ]
            :: content
        )


view : AppContext -> ServiceHash -> Model -> AppDocument Msg
view appContext sh model =
    let
        loading_ =
            ( [ viewLoading ]
            , viewSimpleDescription sh [ Placeholder.view Placeholder.text ]
            , Nothing
            )

        ( content, pageTitleDescription, modal ) =
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
                    ( [ Html.map LogMsg log ]
                    , viewDescription appContext
                        deploy
                        [ ByAt.view appContext.timeZone appContext.now byAt
                        ]
                    , Maybe.map (Modal.map LogMsg) logModal
                    )

                Failure e ->
                    ( [ viewError e ], viewSimpleDescription sh [], Nothing )

        pageTitle =
            PageTitle.title "Service Deploy"
                |> PageTitle.withDescription_ pageTitleDescription

        tabList =
            TabList.tabList [] (TabList.tab "Activity" (Link.serviceDeploy sh)) []

        page =
            PageLayout.tabbedLayout
                pageTitle
                tabList
                (PageContent.oneColumn content)
                (PageLayout.PageFooter [])
                |> PageLayout.withSubduedBackground
    in
    { pageId = "service-deploy-page"
    , title = "Service Deploy: " ++ ServiceHash.toShortString sh ++ " | Unison Cloud"
    , appHeader = Appheader.appHeader
    , page = PageLayout.view page
    , modal = Maybe.map Modal.view modal
    }
