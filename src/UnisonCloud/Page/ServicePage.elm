module UnisonCloud.Page.ServicePage exposing (..)

import Html exposing (Html, div, text)
import Html.Attributes exposing (class)
import Http
import Lib.HttpApi as HttpApi
import RemoteData exposing (RemoteData(..), WebData)
import UI
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
import UnisonCloud.Log as Log
import UnisonCloud.Service as Service exposing (Service)
import UnisonCloud.Service.ServiceName as ServiceName exposing (ServiceName)



-- MODEL


type alias Model =
    { service : WebData Service
    , log : Log.Model
    }


init : AppContext -> ServiceName -> ( Model, Cmd Msg )
init appContext serviceName =
    let
        ( log, logCmd ) =
            Log.init appContext (Log.ServiceContext serviceName)
    in
    ( { service = Loading, log = log }
    , Cmd.batch [ fetchService appContext serviceName, Cmd.map LogMsg logCmd ]
    )



-- UPDATE


type Msg
    = FetchServiceFinished (WebData Service)
    | LogMsg Log.Msg


update : AppContext -> ServiceName -> Msg -> Model -> ( Model, Cmd Msg )
update appContext serviceName msg model =
    case msg of
        FetchServiceFinished service ->
            ( { model | service = service }, Cmd.none )

        LogMsg logMsg ->
            let
                ( log, logCmd ) =
                    Log.update appContext
                        (Log.ServiceContext serviceName)
                        logMsg
                        model.log
            in
            ( { model | log = log }, Cmd.map LogMsg logCmd )



-- EFFECTS


fetchService : AppContext -> ServiceName -> Cmd Msg
fetchService appContext serviceName =
    CloudApi.service serviceName
        |> HttpApi.toRequest
            Service.decode
            (RemoteData.fromResult >> FetchServiceFinished)
        |> HttpApi.perform appContext.api



-- VIEW


viewLoading : Html msg
viewLoading =
    Log.viewLoading


viewError : Http.Error -> Html msg
viewError _ =
    ErrorCard.errorCard
        "Couldn't load service"
        "Something unexpected happened on our end when loading the service and we can't display it."
        |> ErrorCard.toCard
        |> Card.view


viewDescription : ServiceName -> List (Html msg) -> Html msg
viewDescription serviceName content =
    div [ class "service_description" ]
        (text (ServiceName.toString serviceName) :: content)


view : AppContext -> ServiceName -> Model -> AppDocument Msg
view appContext serviceName model =
    let
        loading_ =
            ( [ viewLoading ]
            , viewDescription serviceName [ Placeholder.view Placeholder.text ]
            )

        ( content, pageTitleDescription ) =
            case model.service of
                NotAsked ->
                    loading_

                Loading ->
                    loading_

                Success service ->
                    let
                        log =
                            Log.view appContext model.log

                        byAt =
                            case service.latestDeploy of
                                Just deploy ->
                                    ByAt.byAt deploy.deployedBy deploy.deployedAt
                                        |> ByAt.view appContext.timeZone appContext.now

                                Nothing ->
                                    UI.nothing
                    in
                    ( [ Html.map LogMsg log ]
                    , viewDescription serviceName [ byAt ]
                    )

                Failure e ->
                    ( [ viewError e ], viewDescription serviceName [] )

        pageTitle =
            PageTitle.title (ServiceName.toString serviceName)
                |> PageTitle.withDescription_ pageTitleDescription

        tabList =
            TabList.tabList
                []
                (TabList.tab "Activity" Link.website)
                []

        page =
            PageLayout.tabbedLayout
                pageTitle
                tabList
                (PageContent.oneColumn content)
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
