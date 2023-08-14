module UnisonCloud.Page.ServicesPage exposing (..)

import Html exposing (Html, div, h2, text)
import Html.Attributes exposing (class)
import Http
import Json.Decode as Decode
import Lib.HttpApi as HttpApi
import RemoteData exposing (RemoteData(..), WebData)
import UI
import UI.AppDocument exposing (AppDocument)
import UI.Button as Button
import UI.Card as Card
import UI.DateTime as DateTime
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
import UnisonCloud.Api as CloudApi
import UnisonCloud.AppContext exposing (AppContext)
import UnisonCloud.AppHeader as Appheader
import UnisonCloud.Link as Link
import UnisonCloud.Service as Service exposing (Service)
import UnisonCloud.ServiceDeploy as ServiceDeploy exposing (ServiceDeploy)
import UnisonCloud.ServiceHash as ServiceHash



-- MODEL


type ServicesModal
    = NoModal
    | GetStartedModal


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
        ( heading, latestDeploy ) =
            case service.latestDeploy of
                Just d ->
                    ( Link.view service.name (Link.serviceDeploy d.hash)
                    , div [ class "latest-deploy" ]
                        [ StatusBanner.good (ServiceHash.toString d.hash)
                        , DateTime.view DateTime.Distance d.deployedAt
                        ]
                    )

                Nothing ->
                    ( text service.name, UI.nothing )
    in
    Card.card [ h2 [] [ heading ], latestDeploy ]
        |> Card.asContained
        |> Card.view


viewUnassignedDeploys : List ServiceDeploy -> Html msg
viewUnassignedDeploys deploys =
    let
        viewUnassignedDeploy d =
            Card.card
                [ h2 []
                    [ text (ServiceHash.toString d.hash)
                    ]
                , div [ class "latest-deploy" ]
                    [ StatusBanner.good (ServiceHash.toString d.hash)
                    , DateTime.view DateTime.Distance d.deployedAt
                    ]
                ]
                |> Card.asContained
                |> Card.view
    in
    div [ class "unassigned-deploys" ]
        [ UI.divider
        , h2 []
            [ text "Unassigned Service Deploys" ]
        , div [] (List.map viewUnassignedDeploy deploys)
        ]


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


viewEmptyState : Html Msg
viewEmptyState =
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
                            [ viewEmptyState ]

                        ( [], _ ) ->
                            [ viewUnassignedDeploys deploys ]

                        ( _, [] ) ->
                            List.map viewService services

                        _ ->
                            List.map viewService services
                                ++ [ viewUnassignedDeploys deploys ]

                Failure e ->
                    [ viewError e ]

        modal =
            case model.modal of
                NoModal ->
                    Nothing

                GetStartedModal ->
                    Just viewGetStartedModal

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
