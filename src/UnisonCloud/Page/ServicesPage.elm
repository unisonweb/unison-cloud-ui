module UnisonCloud.Page.ServicesPage exposing (..)

import Html exposing (Html, div, h2, text)
import Html.Attributes exposing (class)
import Json.Decode as Decode
import Lib.HttpApi as HttpApi
import RemoteData exposing (RemoteData(..), WebData)
import UI
import UI.AppDocument exposing (AppDocument)
import UI.Card as Card
import UI.DateTime as DateTime
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


type alias Model =
    WebData (List Service)


init : AppContext -> ( Model, Cmd Msg )
init appContext =
    ( Loading, fetchServices appContext )



-- UPDATE


type Msg
    = FetchServicesFinished (WebData (List Service))


update : AppContext -> Msg -> Model -> ( Model, Cmd Msg )
update _ msg _ =
    case msg of
        FetchServicesFinished services ->
            ( services, Cmd.none )



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


viewLoading : Html msg
viewLoading =
    text "Loading"


view : Model -> AppDocument msg
view model =
    let
        content =
            case model of
                NotAsked ->
                    [ viewLoading ]

                Loading ->
                    [ viewLoading ]

                Success services ->
                    List.map viewService services

                Failure _ ->
                    [ text "Could not load services" ]

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
    , modal = Nothing
    }
