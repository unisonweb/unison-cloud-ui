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
import UnisonCloud.Page.ServicePage.ServiceActivityPage as ServiceActivityPage
import UnisonCloud.Page.ServicePage.ServiceDeploysPage as ServiceDeploysPage
import UnisonCloud.Route as Route exposing (ServiceRoute)
import UnisonCloud.Service as Service exposing (Service)
import UnisonCloud.Service.ServiceName as ServiceName exposing (ServiceName)
import UnisonCloud.ServiceHash as ServiceHash



-- MODEL


type SubPage
    = Activity ServiceActivityPage.Model
    | Deploys ServiceDeploysPage.Model


type alias Model =
    { service : WebData Service
    , subPage : SubPage
    }


init : AppContext -> ServiceName -> ServiceRoute -> ( Model, Cmd Msg )
init appContext serviceName serviceRoute =
    let
        ( subPage, subPageCmd ) =
            case serviceRoute of
                Route.Activity ->
                    let
                        ( activity, activityCmd ) =
                            ServiceActivityPage.init appContext serviceName
                    in
                    ( Activity activity, Cmd.map ServiceActivityPageMsg activityCmd )

                Route.Deploys ->
                    let
                        ( deploys, deploysCmd ) =
                            ServiceDeploysPage.init appContext serviceName
                    in
                    ( Deploys deploys, Cmd.map ServiceDeploysPageMsg deploysCmd )
    in
    ( { service = Loading, subPage = subPage }
    , Cmd.batch [ fetchService appContext serviceName, subPageCmd ]
    )



-- UPDATE


type Msg
    = FetchServiceFinished (WebData Service)
    | ServiceActivityPageMsg ServiceActivityPage.Msg
    | ServiceDeploysPageMsg ServiceDeploysPage.Msg


update : AppContext -> ServiceName -> Msg -> Model -> ( Model, Cmd Msg )
update appContext serviceName msg model =
    case ( msg, model.subPage ) of
        ( FetchServiceFinished service, _ ) ->
            ( { model | service = service }, Cmd.none )

        ( ServiceActivityPageMsg activityMsg, Activity activity ) ->
            let
                ( activity_, activityCmd ) =
                    ServiceActivityPage.update appContext
                        serviceName
                        activityMsg
                        activity
            in
            ( { model | subPage = Activity activity_ }
            , Cmd.map ServiceActivityPageMsg activityCmd
            )

        ( ServiceDeploysPageMsg deploysMsg, Deploys deploys ) ->
            let
                ( deploys_, deploysCmd ) =
                    ServiceDeploysPage.update appContext
                        serviceName
                        deploysMsg
                        deploys
            in
            ( { model | subPage = Deploys deploys_ }
            , Cmd.map ServiceDeploysPageMsg deploysCmd
            )

        _ ->
            ( model, Cmd.none )



-- EFFECTS


fetchService : AppContext -> ServiceName -> Cmd Msg
fetchService appContext serviceName =
    CloudApi.service serviceName
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
            ServiceDeploysPage.viewLoading


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


view : AppContext -> ServiceName -> Model -> AppDocument Msg
view appContext serviceName model =
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
                        (TabList.tab "Activity" (Link.serviceActivity serviceName))
                        [ TabList.tab "Deploys" (Link.serviceDeploysForService serviceName) ]

                Deploys _ ->
                    TabList.tabList
                        [ TabList.tab "Activity" (Link.serviceActivity serviceName) ]
                        (TabList.tab "Deploys" (Link.serviceDeploysForService serviceName))
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
                                        (ServiceActivityPage.view appContext serviceName activity)

                                Deploys deploys ->
                                    PageContent.map ServiceDeploysPageMsg
                                        (ServiceDeploysPage.view appContext serviceName deploys)
                    in
                    ( subPage, description )

                Failure e ->
                    ( PageContent.oneColumn [ viewError e ], viewDescription [] )

        pageTitle =
            PageTitle.title (ServiceName.toString serviceName)
                |> PageTitle.withDescription_ pageTitleDescription

        page =
            PageLayout.tabbedLayout
                pageTitle
                tabList
                content
                (PageLayout.PageFooter [])
    in
    { pageId = "service-page"
    , title = "Service: " ++ ServiceName.toString serviceName ++ " | Unison Cloud"
    , announcement = Nothing
    , appHeader = Appheader.appHeader
    , pageHeader = Nothing
    , page = PageLayout.view page
    , modal = Nothing
    }
