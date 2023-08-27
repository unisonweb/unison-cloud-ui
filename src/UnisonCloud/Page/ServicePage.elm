module UnisonCloud.Page.ServicePage exposing (..)

import Html exposing (Html, div, text)
import Html.Attributes exposing (class)
import Http
import Lib.HttpApi as HttpApi
import RemoteData exposing (RemoteData(..), WebData)
import UI.AppDocument exposing (AppDocument)
import UI.ByAt as ByAt
import UI.Card as Card
import UI.ErrorCard as ErrorCard
import UI.PageContent as PageContent
import UI.PageLayout as PageLayout
import UI.PageTitle as PageTitle
import UI.Placeholder as Placeholder
import UI.TabList as TabList
import UnisonCloud.Api as CloudApi
import UnisonCloud.AppContext exposing (AppContext)
import UnisonCloud.AppHeader as Appheader
import UnisonCloud.Link as Link
import UnisonCloud.Page.ServicePage.AssignedServiceDeploysPage as AssignedServiceDeploysPage
import UnisonCloud.Page.ServicePage.ServiceActivityPage as ServiceActivityPage
import UnisonCloud.Route as Route exposing (ServiceRoute)
import UnisonCloud.Service as Service exposing (Service)
import UnisonCloud.Service.ServiceId as ServiceId exposing (ServiceId)
import UnisonCloud.ServiceHash as ServiceHash



-- MODEL


type SubPage
    = Activity ServiceActivityPage.Model
    | Deploys AssignedServiceDeploysPage.Model


type alias Model =
    { service : WebData Service
    , subPage : SubPage
    }


init : AppContext -> ServiceId -> ServiceRoute -> ( Model, Cmd Msg )
init appContext serviceId serviceRoute =
    let
        ( subPage, subPageCmd ) =
            case serviceRoute of
                Route.Activity ->
                    let
                        ( activity, activityCmd ) =
                            ServiceActivityPage.init appContext serviceId
                    in
                    ( Activity activity, Cmd.map ServiceActivityPageMsg activityCmd )

                Route.Deploys ->
                    let
                        ( deploys, deploysCmd ) =
                            AssignedServiceDeploysPage.init appContext serviceId
                    in
                    ( Deploys deploys, Cmd.map AssignedServiceDeploysPageMsg deploysCmd )
    in
    ( { service = Loading, subPage = subPage }
    , Cmd.batch [ fetchService appContext serviceId, subPageCmd ]
    )



-- UPDATE


type Msg
    = FetchServiceFinished (WebData Service)
    | ServiceActivityPageMsg ServiceActivityPage.Msg
    | AssignedServiceDeploysPageMsg AssignedServiceDeploysPage.Msg


update : AppContext -> ServiceId -> Msg -> Model -> ( Model, Cmd Msg )
update appContext serviceId msg model =
    case ( msg, model.subPage ) of
        ( FetchServiceFinished service, _ ) ->
            ( { model | service = service }, Cmd.none )

        ( ServiceActivityPageMsg activityMsg, Activity activity ) ->
            let
                ( activity_, activityCmd ) =
                    ServiceActivityPage.update appContext
                        serviceId
                        activityMsg
                        activity
            in
            ( { model | subPage = Activity activity_ }
            , Cmd.map ServiceActivityPageMsg activityCmd
            )

        ( AssignedServiceDeploysPageMsg deploysMsg, Deploys deploys ) ->
            let
                ( deploys_, deploysCmd ) =
                    AssignedServiceDeploysPage.update appContext
                        serviceId
                        deploysMsg
                        deploys
            in
            ( { model | subPage = Deploys deploys_ }
            , Cmd.map AssignedServiceDeploysPageMsg deploysCmd
            )

        _ ->
            ( model, Cmd.none )



-- EFFECTS


fetchService : AppContext -> ServiceId -> Cmd Msg
fetchService appContext serviceId =
    CloudApi.service serviceId
        |> HttpApi.toRequest
            Service.decode
            (RemoteData.fromResult >> FetchServiceFinished)
        |> HttpApi.perform appContext.api



-- VIEW


viewLoading : SubPage -> Html msg
viewLoading subPage =
    case subPage of
        Activity _ ->
            ServiceActivityPage.viewLoading

        Deploys _ ->
            AssignedServiceDeploysPage.viewLoading


viewError : Http.Error -> Html msg
viewError _ =
    ErrorCard.errorCard
        "Couldn't load service"
        "Something unexpected happened on our end when loading the service and we can't display it."
        |> ErrorCard.toCard
        |> Card.view


viewDescription : List (Html msg) -> Html msg
viewDescription content =
    div [ class "service_description" ]
        content


view : AppContext -> ServiceId -> Model -> AppDocument Msg
view appContext serviceId model =
    let
        loading_ =
            ( PageContent.oneColumn [ viewLoading model.subPage ]
            , viewDescription [ Placeholder.view Placeholder.text ]
            )

        tabList =
            case model.subPage of
                Activity _ ->
                    TabList.tabList
                        []
                        (TabList.tab "Activity" (Link.serviceActivity serviceId))
                        [ TabList.tab "Deploys" (Link.serviceDeploysForService serviceId) ]

                Deploys _ ->
                    TabList.tabList
                        [ TabList.tab "Activity" (Link.serviceActivity serviceId) ]
                        (TabList.tab "Deploys" (Link.serviceDeploysForService serviceId))
                        []

        ( content, pageTitleDescription ) =
            case model.service of
                NotAsked ->
                    loading_

                Loading ->
                    loading_

                Success service ->
                    let
                        latestDeploy =
                            case service.latestDeploy of
                                Just deploy ->
                                    [ text (ServiceHash.toShortString deploy.hash)
                                    , ByAt.byAt deploy.deployedBy deploy.deployedAt
                                        |> ByAt.view appContext.timeZone appContext.now
                                    ]

                                Nothing ->
                                    []

                        description =
                            viewDescription latestDeploy

                        subPage =
                            case model.subPage of
                                Activity activity ->
                                    PageContent.map ServiceActivityPageMsg
                                        (ServiceActivityPage.view appContext serviceId activity)

                                Deploys deploys ->
                                    PageContent.map AssignedServiceDeploysPageMsg
                                        (AssignedServiceDeploysPage.view appContext serviceId deploys)
                    in
                    ( subPage, description )

                Failure e ->
                    ( PageContent.oneColumn [ viewError e ], viewDescription [] )

        pageTitle =
            PageTitle.title (ServiceId.toString serviceId)
                |> PageTitle.withDescription_ pageTitleDescription

        page =
            PageLayout.tabbedLayout
                pageTitle
                tabList
                content
                (PageLayout.PageFooter [])
    in
    { pageId = "service-page"
    , title = "Service: " ++ ServiceId.toString serviceId ++ " | Unison Cloud"
    , announcement = Nothing
    , appHeader = Appheader.appHeader
    , pageHeader = Nothing
    , page = PageLayout.view page
    , modal = Nothing
    }
