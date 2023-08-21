module UnisonCloud.Page.ServicesPage exposing (..)

import Html exposing (Html, div, h1, h2, p, text)
import Html.Attributes exposing (class)
import Http
import Json.Decode as Decode
import Lib.HttpApi as HttpApi
import RemoteData exposing (RemoteData(..), WebData)
import Set
import UI
import UI.AppDocument exposing (AppDocument)
import UI.Button as Button
import UI.ByAt as ByAt
import UI.Card as Card
import UI.Click as Click
import UI.EmptyState as EmptyState
import UI.EmptyStateCard as EmptyStateCard
import UI.ErrorCard as ErrorCard
import UI.Icon as Icon
import UI.Modal as Modal
import UI.PageContent as PageContent
import UI.PageLayout as PageLayout
import UI.PageTitle as PageTitle
import UI.Placeholder as Placeholder
import UI.StatusBanner as StatusBanner
import UI.Tag as Tag
import UnisonCloud.Api as CloudApi
import UnisonCloud.AppContext exposing (AppContext)
import UnisonCloud.AppHeader as Appheader
import UnisonCloud.Link as Link
import UnisonCloud.Service as Service exposing (Service)
import UnisonCloud.Service.ServiceName as ServiceName
import UnisonCloud.ServiceDeploy as ServiceDeploy exposing (ServiceDeploy)
import UnisonCloud.ServiceHash as ServiceHash



-- MODEL


type ServicesModal
    = NoModal
    | GetStartedModal
    | AssignmentGuideModal


type alias Model =
    { services : WebData (List Service)
    , unassignedDeploys : WebData (List ServiceDeploy)
    , modal : ServicesModal
    }


init : AppContext -> ( Model, Cmd Msg )
init appContext =
    ( { services = Loading, unassignedDeploys = Loading, modal = NoModal }
    , Cmd.batch
        [ fetchServices appContext
        , fetchUnassignedDeploys appContext
        ]
    )



-- UPDATE


type Msg
    = FetchServicesFinished (WebData (List Service))
    | FetchUnassignedDeploysFinished (WebData (List ServiceDeploy))
    | ShowGetStartedModal
    | ShowAssignmentGuideModal
    | CloseModal


update : AppContext -> Msg -> Model -> ( Model, Cmd Msg )
update _ msg model =
    case msg of
        FetchServicesFinished services ->
            ( { model | services = services }, Cmd.none )

        FetchUnassignedDeploysFinished deploys ->
            ( { model | unassignedDeploys = deploys }, Cmd.none )

        ShowGetStartedModal ->
            ( { model | modal = GetStartedModal }, Cmd.none )

        ShowAssignmentGuideModal ->
            ( { model | modal = AssignmentGuideModal }, Cmd.none )

        CloseModal ->
            ( { model | modal = NoModal }, Cmd.none )



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
            (Decode.list ServiceDeploy.decode)
            (RemoteData.fromResult >> FetchUnassignedDeploysFinished)
        |> HttpApi.perform appContext.api



-- VIEW


viewService : Service -> Html msg
viewService service =
    let
        heading =
            Link.view (ServiceName.toString service.name) (Link.service service.name)

        latestDeploy =
            case service.latestDeploy of
                Just d ->
                    div [ class "latest-deploy" ]
                        [ StatusBanner.good (ServiceHash.toShortString d.hash)
                        , ByAt.view (ByAt.byAt d.deployedBy d.deployedAt)
                        ]

                Nothing ->
                    text "🐣 No deploys yet"

        tags =
            if Set.isEmpty service.tags then
                UI.nothing

            else
                service.tags
                    |> Set.toList
                    |> List.map Tag.tag
                    |> Tag.viewTags
    in
    Card.card [ h2 [] [ heading ], tags, latestDeploy ]
        |> Card.asContained
        |> Card.view


viewUnassignedDeploys : Bool -> List ServiceDeploy -> Html Msg
viewUnassignedDeploys hasServices deploys =
    let
        viewUnassignedDeploy d =
            Card.card
                [ Click.view []
                    [ h2 [] [ text (ServiceHash.toShortString d.hash) ] ]
                    (Link.serviceDeploy d.hash)
                , ByAt.view (ByAt.byAt d.deployedBy d.deployedAt)
                ]
                |> Card.asContained
                |> Card.view

        howToOrganizeBlurb =
            if hasServices then
                p [ class "unassigned-deploys_learn-how-to-organize" ]
                    [ text "Organize your deploys by assigning them to a service."
                    , Button.button ShowAssignmentGuideModal "Learn how"
                        |> Button.small
                        |> Button.view
                    ]

            else
                UI.nothing
    in
    div [ class "unassigned-deploys" ]
        ([ h1 [] [ text "Ad-hoc Service Deploys" ]
         , howToOrganizeBlurb
         ]
            ++ List.map viewUnassignedDeploy deploys
        )


viewGetStartedModal : Html Msg
viewGetStartedModal =
    let
        installDependencies =
            """.> project.create helloWorld
helloWorld/main> pull @unison/cloud/latest lib.cloud"""

        program =
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
    deployHttp env (pool.wrap (Route.run server) )"""

        runCmd =
            "helloWorld/main> run helloWorld"

        content =
            div [ class "get-started-modal" ]
                [ UI.codeBlock [] (text installDependencies)
                , UI.codeBlock [] (text program)
                , UI.codeBlock [] (text runCmd)
                ]
    in
    content
        |> Modal.content
        |> Modal.modal "get-started-modal" CloseModal
        |> Modal.withHeader "Get started with Unison Cloud services"
        |> Modal.withActions
            [ Button.iconThenLabel CloseModal Icon.thumbsUp "Got It"
                |> Button.emphasized
            ]
        |> Modal.view


viewAssignmentGuideModal : Html Msg
viewAssignmentGuideModal =
    let
        content =
            div [ class "assignment-guide-modal" ] [ text "todo" ]
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
        |> Card.asContainedWithFade
        |> Card.view


viewServicesEmptyState : Html Msg
viewServicesEmptyState =
    EmptyState.iconCloud
        (EmptyState.CircleCenterPiece (text "🌤️"))
        |> EmptyState.withContent
            [ h2 [] [ text "Sunny, with a chance of clouds" ]
            , Button.iconThenLabel ShowAssignmentGuideModal
                Icon.graduationCap
                "Assign deployments to services for better organization"
                |> Button.decorativeBlue
                |> Button.view
            ]
        |> EmptyStateCard.view


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
        |> EmptyStateCard.view


view : Model -> AppDocument Msg
view model =
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
                    case ( services, deploys ) of
                        ( [], [] ) ->
                            [ viewCompleteEmptyState ]

                        ( [], _ ) ->
                            [ viewServicesEmptyState, viewUnassignedDeploys False deploys ]

                        ( _, [] ) ->
                            List.map viewService services

                        _ ->
                            List.map viewService services
                                ++ [ viewUnassignedDeploys True deploys ]

                Failure e ->
                    [ viewError e ]

        modal =
            case model.modal of
                NoModal ->
                    Nothing

                GetStartedModal ->
                    Just viewGetStartedModal

                AssignmentGuideModal ->
                    Just viewAssignmentGuideModal

        page =
            PageLayout.centeredLayout
                (PageContent.oneColumn content
                    |> PageContent.withPageTitle (PageTitle.title "Services")
                )
                (PageLayout.PageFooter [])
                |> PageLayout.withSubduedBackground
    in
    { pageId = "services-page"
    , title = "Services | Unison Cloud"
    , announcement = Nothing
    , appHeader = Appheader.appHeader
    , pageHeader = Nothing
    , page = PageLayout.view page
    , modal = modal
    }
