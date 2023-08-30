module UnisonCloud.Page.ServicePage exposing (..)

import Html exposing (Html, div, h1, p, text)
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
import UnisonCloud.Page.ServicePage.AssignedServiceDeployPage as AssignedServiceDeployPage
import UnisonCloud.Page.ServicePage.AssignedServiceDeploysPage as AssignedServiceDeploysPage
import UnisonCloud.Page.ServicePage.ServiceActivityPage as ServiceActivityPage
import UnisonCloud.Route as Route exposing (ServiceRoute)
import UnisonCloud.Service as Service exposing (Service)
import UnisonCloud.Service.ServiceId as ServiceId exposing (ServiceId)
import UnisonCloud.Service.ServiceName as ServiceName
import UnisonCloud.ServiceHash as ServiceHash exposing (ServiceHash)
import Url



-- MODEL


type SubPage
    = Activity ServiceActivityPage.Model
    | Deploy ServiceHash AssignedServiceDeployPage.Model
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

                Route.Deploy hash ->
                    let
                        ( deploy, deployCmd ) =
                            AssignedServiceDeployPage.init appContext serviceId hash
                    in
                    ( Deploy hash deploy, Cmd.map AssignedServiceDeployPageMsg deployCmd )

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
    | AssignedServiceDeployPageMsg AssignedServiceDeployPage.Msg
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

        ( AssignedServiceDeployPageMsg deployMsg, Deploy serviceHash deploy ) ->
            let
                ( deploy_, deployCmd ) =
                    AssignedServiceDeployPage.update appContext
                        serviceId
                        serviceHash
                        deployMsg
                        deploy
            in
            ( { model | subPage = Deploy serviceHash deploy_ }
            , Cmd.map AssignedServiceDeployPageMsg deployCmd
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

        Deploy _ _ ->
            AssignedServiceDeployPage.viewLoading

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
    div [ class "service-description" ]
        content


view : AppContext -> ServiceId -> Model -> AppDocument Msg
view appContext serviceId model =
    let
        loading_ =
            { content = PageContent.oneColumn [ viewLoading model.subPage ]
            , serviceTitle = "Service Loading..."
            , description = viewDescription [ Placeholder.view Placeholder.text ]
            , exposedLink = Nothing
            , modal = Nothing
            }

        tabList =
            case model.subPage of
                Activity _ ->
                    TabList.tabList
                        []
                        (TabList.tab "Activity" (Link.serviceActivity serviceId))
                        [ TabList.tab "Deploys" (Link.serviceDeploysForService serviceId) ]

                Deploy _ _ ->
                    TabList.tabList
                        [ TabList.tab "Activity" (Link.serviceActivity serviceId) ]
                        (TabList.tab "Deploys" (Link.serviceDeploysForService serviceId))
                        []

                Deploys _ ->
                    TabList.tabList
                        [ TabList.tab "Activity" (Link.serviceActivity serviceId) ]
                        (TabList.tab "Deploys" (Link.serviceDeploysForService serviceId))
                        []

        { content, serviceTitle, description, exposedLink, modal } =
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
                                    [ div [ class "service-description_hash" ] [ text (ServiceHash.toShortString deploy.hash) ]
                                    , ByAt.byAt deploy.deployedBy deploy.deployedAt
                                        |> ByAt.view appContext.timeZone appContext.now
                                    ]

                                Nothing ->
                                    []

                        exposedLink_ =
                            service
                                |> Service.exposedUrl appContext
                                |> Maybe.map Url.toString
                                |> Maybe.map Click.externalHref
                                |> Maybe.map ExternalLinkIcon.view

                        description_ =
                            viewDescription latestDeploy
                    in
                    case model.subPage of
                        Activity activity ->
                            let
                                ( serviceActivityPage, activityModal ) =
                                    ServiceActivityPage.view appContext serviceId activity
                            in
                            { content = PageContent.map ServiceActivityPageMsg serviceActivityPage
                            , serviceTitle = ServiceName.toString service.name
                            , description = description_
                            , exposedLink = exposedLink_
                            , modal = Maybe.map (Modal.map ServiceActivityPageMsg) activityModal
                            }

                        Deploy hash deploy ->
                            let
                                deploy_ =
                                    AssignedServiceDeployPage.view appContext serviceId hash deploy

                                exposedDeployLink =
                                    deploy_.exposedUrl
                                        |> Maybe.map Url.toString
                                        |> Maybe.map Click.externalHref
                                        |> Maybe.map ExternalLinkIcon.view
                            in
                            { content = PageContent.map AssignedServiceDeployPageMsg deploy_.content
                            , serviceTitle = ServiceName.toString service.name ++ " > " ++ ServiceHash.toShortString hash
                            , description = deploy_.description
                            , exposedLink = exposedDeployLink
                            , modal = Maybe.map (Modal.map AssignedServiceDeployPageMsg) deploy_.modal
                            }

                        Deploys deploys ->
                            { content =
                                PageContent.map AssignedServiceDeploysPageMsg
                                    (AssignedServiceDeploysPage.view appContext serviceId deploys)
                            , serviceTitle = ServiceName.toString service.name
                            , description = description_
                            , exposedLink = exposedLink_
                            , modal = Nothing
                            }

                Failure e ->
                    { content = PageContent.oneColumn [ viewError e ]
                    , serviceTitle = "Service: " ++ ServiceId.toString serviceId
                    , description = viewDescription []
                    , exposedLink = Nothing
                    , modal = Nothing
                    }

        pageTitle =
            PageTitle.custom
                [ h1 []
                    [ text serviceTitle
                    , Maybe.withDefault UI.nothing exposedLink
                    ]
                , p [ class "description" ] [ description ]
                ]

        page =
            PageLayout.tabbedLayout
                pageTitle
                tabList
                content
                (PageLayout.PageFooter [])
    in
    { pageId = "service-page"
    , title = serviceTitle ++ " | Unison Cloud"
    , appHeader = Appheader.appHeader
    , page = PageLayout.view page
    , modal = Maybe.map Modal.view modal
    }
