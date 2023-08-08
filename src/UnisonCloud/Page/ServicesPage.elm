module UnisonCloud.Page.ServicesPage exposing (..)

import Html exposing (Html, text)
import Json.Decode as Decode
import Lib.HttpApi as HttpApi
import RemoteData exposing (RemoteData(..), WebData)
import Time
import UI.AppDocument exposing (AppDocument)
import UI.Card as Card
import UI.DateTime as DateTime
import UI.PageContent as PageContent
import UI.PageLayout as PageLayout
import UI.PageTitle as PageTitle
import UnisonCloud.Api as CloudApi
import UnisonCloud.AppHeader as Appheader
import UnisonCloud.Env exposing (Env)
import UnisonCloud.Link as Link
import UnisonCloud.Service as Service exposing (Service)
import UnisonCloud.ServiceHash as ServiceHash



-- MODEL


type alias Model =
    WebData (List Service)


init : Env -> ( Model, Cmd Msg )
init _ =
    let
        services =
            [ { id = Service.ServiceId "asdf"
              , hash = ServiceHash.unsafeFromString "1234"
              , type_ = Service.Native
              , deployedAt = DateTime.fromPosix (Time.millisToPosix 1690306324916)
              , undeployedAt = Nothing
              }
            ]
    in
    ( Success services, Cmd.none )



-- fetchServices env )
-- UPDATE


type Msg
    = FetchServicesFinished (WebData (List Service))


update : Env -> Msg -> Model -> ( Model, Cmd Msg )
update _ msg _ =
    case msg of
        FetchServicesFinished services ->
            ( services, Cmd.none )



-- EFFECTS


fetchServices : Env -> Cmd Msg
fetchServices env =
    CloudApi.services
        |> HttpApi.toRequest
            (Decode.list Service.decode)
            (RemoteData.fromResult >> FetchServicesFinished)
        |> HttpApi.perform env.api



-- VIEW


viewService : Service -> Html msg
viewService service =
    Card.card [ Link.view (ServiceHash.toString service.hash) (Link.serviceDeploy service.hash) ]
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
