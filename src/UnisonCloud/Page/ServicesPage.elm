module UnisonCloud.Page.ServicesPage exposing (..)

import Html exposing (Html, div, h2, text)
import Html.Attributes exposing (class)
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
import UI.Icon as Icon
import UI.Modal as Modal
import UI.PageContent as PageContent
import UI.PageLayout as PageLayout
import UI.PageTitle as PageTitle
import UI.StatusBanner as StatusBanner
import UnisonCloud.Api as CloudApi
import UnisonCloud.AppContext exposing (AppContext)
import UnisonCloud.AppHeader as Appheader
import UnisonCloud.Link as Link
import UnisonCloud.Service as Service exposing (Service)
import UnisonCloud.ServiceHash as ServiceHash



-- MODEL


type ServicesModal
    = NoModal
    | GetStartedModal


type alias Model =
    { services : WebData (List Service)
    , modal : ServicesModal
    }


init : AppContext -> ( Model, Cmd Msg )
init appContext =
    ( { services = Loading, modal = NoModal }, fetchServices appContext )



-- UPDATE


type Msg
    = FetchServicesFinished (WebData (List Service))
    | ShowGetStartedModal
    | CloseModal


update : AppContext -> Msg -> Model -> ( Model, Cmd Msg )
update _ msg model =
    case msg of
        FetchServicesFinished services ->
            ( { model | services = services }, Cmd.none )

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



{-

-}


viewGetStartedModal : Html Msg
viewGetStartedModal =
    let
        installDependencies =
            ""

        program =
            """
main : '{IO, Exception} ServiceHash HttpRequest HttpResponse
main = do
  server : '{Route, Remote} ()
  server =
    getHello = do
      _ = noCapture GET (s "" )
      ok.text ("Hello World!")

    getHello

  Cloud.run do
    env = Environment.create "hello-world-production"
    deployHttp env (pool.wrap (Route.run server) )
          """

        runCmd =
            "run main"

        content =
            div []
                [ div [] [ text installDependencies ]
                , div [] [ text program ]
                , div [] [ text runCmd ]
                ]
    in
    content
        |> Modal.Content
        |> Modal.modal "get-started-modal" CloseModal
        |> Modal.view


viewLoading : Html msg
viewLoading =
    text "Loading"


viewError : Html msg
viewError =
    text "Could not load services"


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
        content =
            case model.services of
                NotAsked ->
                    [ viewLoading ]

                Loading ->
                    [ viewLoading ]

                Success services ->
                    case services of
                        [] ->
                            [ viewEmptyState ]

                        _ ->
                            List.map viewService services

                Failure _ ->
                    [ viewError ]

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
