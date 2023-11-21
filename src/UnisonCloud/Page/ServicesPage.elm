module UnisonCloud.Page.ServicesPage exposing (..)

import Html exposing (Html, div, h2, i, p, span, text)
import Html.Attributes exposing (class)
import Http
import Json.Decode as Decode
import Lib.HttpApi as HttpApi
import Lib.Util as Util
import RemoteData exposing (RemoteData(..), WebData)
import Set
import UI
import UI.Button as Button
import UI.ByAt as ByAt
import UI.Card as Card
import UI.Click as Click
import UI.DateTime as DateTime
import UI.EmptyState as EmptyState
import UI.EmptyStateCard as EmptyStateCard
import UI.ErrorCard as ErrorCard
import UI.ExternalLinkIcon as ExternalLinkIcon
import UI.Icon as Icon
import UI.Modal as Modal
import UI.PageContent as PageContent
import UI.PageLayout as PageLayout
import UI.PageTitle as PageTitle
import UI.Placeholder as Placeholder
import UI.TabList as TabList
import UI.Tag as Tag
import UnisonCloud.Api as CloudApi
import UnisonCloud.AppContext exposing (AppContext)
import UnisonCloud.AppDocument exposing (AppDocument)
import UnisonCloud.AppHeader as Appheader
import UnisonCloud.Link as Link
import UnisonCloud.Service as Service exposing (Service)
import UnisonCloud.Service.ServiceName as ServiceName
import UnisonCloud.ServiceDeploy as ServiceDeploy exposing (ServiceDeploySummary)
import UnisonCloud.ServiceDeploySettings as ServiceDeploySettings
import UnisonCloud.ServiceHash as ServiceHash
import Url



-- MODEL


type ServicesModal
    = NoModal
    | GetStartedModal
    | AssignmentGuideModal


type ActiveTab
    = NamedServices
    | AdHocDeploys


type alias Model =
    { services : WebData (List Service)
    , unassignedDeploys : WebData (List ServiceDeploySummary)
    , activeTab : ActiveTab
    , modal : ServicesModal
    , serviceDeploySettings : ServiceDeploySettings.Model
    }


init : AppContext -> ( Model, Cmd Msg )
init appContext =
    ( { services = Loading
      , unassignedDeploys = Loading
      , activeTab = NamedServices
      , modal = NoModal
      , serviceDeploySettings = ServiceDeploySettings.init
      }
    , Cmd.batch
        [ fetchServices appContext
        , fetchUnassignedDeploys appContext
        ]
    )



-- UPDATE


type Msg
    = FetchServicesFinished (WebData (List Service))
    | FetchUnassignedDeploysFinished (WebData (List ServiceDeploySummary))
    | SetActiveTab ActiveTab
    | ShowGetStartedModal
    | ShowAssignmentGuideModal
    | ServiceDeploySettingsMsg ServiceDeploySettings.Msg
    | CloseModal


update : AppContext -> Msg -> Model -> ( Model, Cmd Msg )
update appContext msg model =
    case msg of
        FetchServicesFinished services ->
            ( { model | services = services }, Cmd.none )

        FetchUnassignedDeploysFinished deploys ->
            let
                deploys_ =
                    deploys
                        |> RemoteData.map
                            (Util.sortByWith
                                (.deployedAt >> DateTime.toISO8601)
                                (Util.descending compare)
                            )
                        |> RemoteData.map (List.filter ServiceDeploy.isLive)
            in
            ( { model | unassignedDeploys = deploys_ }, Cmd.none )

        SetActiveTab newActiveTab ->
            ( { model | activeTab = newActiveTab }, Cmd.none )

        ShowGetStartedModal ->
            ( { model | modal = GetStartedModal }, Cmd.none )

        ShowAssignmentGuideModal ->
            ( { model | modal = AssignmentGuideModal }, Cmd.none )

        CloseModal ->
            ( { model | modal = NoModal }, Cmd.none )

        ServiceDeploySettingsMsg spMsg ->
            let
                ( serviceDeploySettings, cmd, out ) =
                    ServiceDeploySettings.update
                        appContext
                        spMsg
                        model.serviceDeploySettings

                ( unassignedDeploys, outCmd ) =
                    case out of
                        ServiceDeploySettings.UndeployedServiceDeploy sh ->
                            ( model.unassignedDeploys
                                |> RemoteData.map (List.filter (.hash >> ServiceHash.equals sh >> not))
                            , Cmd.none
                            )

                        ServiceDeploySettings.AssignedToService sh ->
                            ( model.unassignedDeploys
                                |> RemoteData.map (List.filter (.hash >> ServiceHash.equals sh >> not))
                            , fetchServices appContext
                            )

                        _ ->
                            ( model.unassignedDeploys, Cmd.none )
            in
            ( { model
                | serviceDeploySettings = serviceDeploySettings
                , unassignedDeploys = unassignedDeploys
              }
            , Cmd.batch [ Cmd.map ServiceDeploySettingsMsg cmd, outCmd ]
            )



-- EFFECTS


fetchServices : AppContext -> Cmd Msg
fetchServices appContext =
    CloudApi.services
        |> HttpApi.toRequest
            (Decode.list Service.decode)
            (RemoteData.fromResult >> FetchServicesFinished)
        |> HttpApi.perform appContext.api


fetchUnassignedDeploys : AppContext -> Cmd Msg
fetchUnassignedDeploys appContext =
    CloudApi.unassignedServiceDeploys
        |> HttpApi.toRequest
            (Decode.list ServiceDeploy.decodeSummary)
            (RemoteData.fromResult >> FetchUnassignedDeploysFinished)
        |> HttpApi.perform appContext.api



-- VIEW


viewService : AppContext -> Service -> Html msg
viewService appContext service =
    let
        heading =
            Link.view (ServiceName.toString service.name) (Link.service service.name)

        exposedLink =
            case Service.exposedUrl appContext service of
                Just url ->
                    ExternalLinkIcon.view
                        (Click.externalHref (Url.toString url))

                Nothing ->
                    UI.nothing

        activeDeploy =
            case service.activeDeploy of
                Just d ->
                    Click.view [ class "active-deploy" ]
                        [ div [ class "active-deploy_active-hash" ] [ text (ServiceHash.toShortString d.hash) ]
                        , ByAt.view appContext.timeZone appContext.now (ByAt.byAt d.deployedBy d.deployedAt)
                        ]
                        (Link.serviceDeployForService service.name d.hash)

                Nothing ->
                    div [ class "no-deploys-yet" ] [ text "🐣 No deploys yet" ]

        tags =
            if Set.isEmpty service.tags then
                UI.nothing

            else
                service.tags
                    |> Set.toList
                    |> List.map Tag.tag
                    |> Tag.viewTags
    in
    Card.card [ h2 [] [ heading, exposedLink ], tags, activeDeploy ]
        |> Card.withClassName "named-service"
        |> Card.asContained
        |> Card.view


viewUnassignedDeploys :
    AppContext
    -> Bool
    -> List ServiceDeploySummary
    -> ServiceDeploySettings.Model
    -> Html Msg
viewUnassignedDeploys appContext hasServices deploys serviceDeploySettings =
    let
        exposedLink d =
            case ServiceDeploy.exposedUrl appContext d of
                Just url ->
                    ExternalLinkIcon.view
                        (Click.externalHref (Url.toString url))

                Nothing ->
                    UI.nothing

        viewUnassignedDeploy d =
            let
                settingsMenu =
                    ServiceDeploySettings.viewMenu
                        { isAssignable = True, iconButton = True }
                        d.hash
                        serviceDeploySettings
            in
            div [ class "unassigned-deploy-row" ]
                [ span [ class "unassigned-deploy-row_hash" ]
                    [ Click.view []
                        [ text (ServiceHash.toShortString d.hash)
                        ]
                        (Link.serviceDeploy d.hash)
                    , exposedLink d
                    ]
                , ByAt.view appContext.timeZone appContext.now (ByAt.byAt d.deployedBy d.deployedAt)
                , div [ class "unassigned-deploy-row_right-side" ]
                    [ Html.map ServiceDeploySettingsMsg settingsMenu
                    ]
                ]

        howToOrganizeBlurb =
            if hasServices then
                p [ class "unassigned-deploys_learn-how-to-organize" ]
                    [ text "Organize your deploys by assigning them to a named service."
                    , Button.iconThenLabel ShowAssignmentGuideModal Icon.graduationCap "Learn how"
                        |> Button.small
                        |> Button.view
                    ]

            else
                UI.nothing
    in
    div [ class "unassigned-deploys" ]
        [ div [ class "unassigned-deploys_header" ]
            [ howToOrganizeBlurb ]
        , Card.card (List.map viewUnassignedDeploy deploys)
            |> Card.asContained
            |> Card.view
        ]


viewGetStartedModal : Html Msg
viewGetStartedModal =
    let
        getStarted =
            """.> project.create-empty
amusing-giraffe/main> pull @unison/cloud-start/releases/latest
amusing-giraffe/main> run examples.helloWorld.deploy"""

        content =
            div [ class "get-started-modal" ]
                [ p [] [ text "Here's how to get started with a small ", i [] [ text "hello world" ], text " template service:" ]
                , UI.codeBlock [] (text getStarted)
                ]
    in
    content
        |> Modal.content
        |> Modal.modal "get-started-modal" CloseModal
        |> Modal.withHeader "Get started with Unison Cloud services"
        |> Modal.withLeftSideFooter
            [ div []
                [ text "Learn more in the "
                , Link.view "Cloud Start project documentation." Link.cloudStartDocs
                ]
            ]
        |> Modal.withActions
            [ Button.iconThenLabel CloseModal Icon.thumbsUp "Got It"
                |> Button.emphasized
            ]
        |> Modal.view


viewAssignmentGuideModal : Html Msg
viewAssignmentGuideModal =
    let
        assignment =
            """helloWorld : '{IO, Exception} ServiceHash HttpRequest HttpResponse
helloWorld = do
  server : '{Route, Remote} ()
  server =
    getHello = do
      _ = noCapture GET (s "" )
      ok.text ("Hello World!")

    getHello

  Cloud.run do
    env = Environment.create "hello-world-production"
    serviceName = ServiceName.create "hello-world"
    serviceHash = deployHttp env (pool.wrap (Route.run server) )
    ServiceName.assign serviceName serviceHash
    serviceHash"""

        content =
            div [ class "assignment-guide-modal" ]
                [ p [] [ text "Assigning service deployments to a name allows them to get a stable URL." ]
                , p [] [ text "Here's how to assign a name to a small ", i [] [ text "hello world" ], text " service:" ]
                , UI.codeBlock [] (text assignment)
                ]
    in
    content
        |> Modal.content
        |> Modal.modal "assignment-guide-modal" CloseModal
        |> Modal.withHeader "Assigning deployments to services"
        |> Modal.withActions
            [ Button.iconThenLabel CloseModal Icon.thumbsUp "Got It"
                |> Button.emphasized
            ]
        |> Modal.view


viewLoading : List (Html msg)
viewLoading =
    let
        placeholder_ length intensity =
            Placeholder.text |> Placeholder.withLength length |> Placeholder.withIntensity intensity |> Placeholder.view

        placeholders =
            [ placeholder_ Placeholder.Medium Placeholder.Normal
            , placeholder_ Placeholder.Small Placeholder.Subdued
            , placeholder_ Placeholder.Large Placeholder.Subdued
            , placeholder_ Placeholder.Medium Placeholder.Subdued
            ]

        viewCard_ =
            Card.card placeholders
                |> Card.asContained
                |> Card.view
    in
    [ viewCard_, viewCard_, viewCard_ ]


viewError : Http.Error -> Html msg
viewError _ =
    ErrorCard.errorCard
        "Couldn't load services"
        "Something unexpected happened on our end when loading services and we can't display them."
        |> ErrorCard.toCard
        |> Card.asContainedWithFade_ Card.SurfaceBackground
        |> Card.view


viewServicesEmptyState : Html Msg
viewServicesEmptyState =
    EmptyState.iconCloud
        (EmptyState.CircleCenterPiece (text "🌤️"))
        |> EmptyState.withContent
            [ h2 [] [ text "Sunny, with a chance of clouds" ]
            , Button.iconThenLabel ShowAssignmentGuideModal
                Icon.graduationCap
                "Assign deployments to named services for better organization"
                |> Button.decorativeBlue
                |> Button.view
            ]
        |> EmptyStateCard.view_ Card.SurfaceBackground


viewUnassignedDeploysEmptyState : Html Msg
viewUnassignedDeploysEmptyState =
    EmptyState.iconCloud
        (EmptyState.CircleCenterPiece (text "🌤️"))
        |> EmptyState.withContent
            [ h2 [] [ text "Sunny, with a chance of clouds" ]
            , p [] [ text "Ad hoc services are useful when testing out ideas, before they feel fully ready to be named." ]
            ]
        |> EmptyStateCard.view_ Card.SurfaceBackground


viewCompleteEmptyState : Html Msg
viewCompleteEmptyState =
    EmptyState.iconCloud
        (EmptyState.CircleCenterPiece (text "🌤️"))
        |> EmptyState.withContent
            [ h2 [] [ text "Sunny, with a chance of clouds" ]
            , Button.iconThenLabel ShowGetStartedModal Icon.graduationCap "Get started with a \"Hello World\" Cloud service"
                |> Button.decorativeBlue
                |> Button.view
            ]
        |> EmptyStateCard.view_ Card.SurfaceBackground


view : AppContext -> Model -> AppDocument Msg
view appContext model =
    let
        data =
            RemoteData.map2
                Tuple.pair
                model.services
                model.unassignedDeploys

        content =
            case data of
                NotAsked ->
                    viewLoading

                Loading ->
                    viewLoading

                Success ( services, deploys ) ->
                    case model.activeTab of
                        NamedServices ->
                            case ( services, deploys ) of
                                ( [], [] ) ->
                                    [ viewCompleteEmptyState ]

                                ( [], _ ) ->
                                    [ viewServicesEmptyState ]

                                _ ->
                                    List.map (viewService appContext) services

                        AdHocDeploys ->
                            case ( services, deploys ) of
                                ( [], [] ) ->
                                    [ viewCompleteEmptyState ]

                                ( _, [] ) ->
                                    [ viewUnassignedDeploysEmptyState ]

                                _ ->
                                    [ viewUnassignedDeploys appContext True deploys model.serviceDeploySettings ]

                Failure e ->
                    [ viewError e ]

        modal =
            case model.modal of
                NoModal ->
                    model.serviceDeploySettings
                        |> ServiceDeploySettings.viewModal
                        |> Maybe.map (Modal.map ServiceDeploySettingsMsg)
                        |> Maybe.map Modal.view

                GetStartedModal ->
                    Just viewGetStartedModal

                AssignmentGuideModal ->
                    Just viewAssignmentGuideModal

        tabList =
            case model.activeTab of
                NamedServices ->
                    TabList.tabList []
                        (TabList.tab "Named Services" (Click.onClick (SetActiveTab NamedServices)))
                        [ TabList.tab "Ad hoc deploys" (Click.onClick (SetActiveTab AdHocDeploys)) ]

                AdHocDeploys ->
                    TabList.tabList
                        [ TabList.tab "Named Services" (Click.onClick (SetActiveTab NamedServices)) ]
                        (TabList.tab "Ad hoc deploys" (Click.onClick (SetActiveTab AdHocDeploys)))
                        []

        page =
            PageLayout.tabbedLayout
                (PageTitle.title "Cloud Services")
                tabList
                (PageContent.oneColumn content)
                (PageLayout.PageFooter [])
                |> PageLayout.withSubduedBackground
    in
    { pageId = "services-page"
    , title = "Cloud Services | Unison Cloud"
    , appHeader = Appheader.appHeader
    , page = PageLayout.view page
    , modal = modal
    }
