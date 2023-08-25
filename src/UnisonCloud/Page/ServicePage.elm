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
import UnisonCloud.Page.ServicePage.ServiceActivityPage as ServiceActivityPage
import UnisonCloud.Route as Route exposing (ServiceRoute(..))
import UnisonCloud.Service as Service exposing (Service)
import UnisonCloud.Service.ServiceName as ServiceName exposing (ServiceName)



-- MODEL


type SubPage
    = Activity ServiceActivityPage.Model


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
    in
    ( { service = Loading, subPage = subPage }
    , Cmd.batch [ fetchService appContext serviceName, subPageCmd ]
    )



-- UPDATE


type Msg
    = FetchServiceFinished (WebData Service)
    | ServiceActivityPageMsg ServiceActivityPage.Msg


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
            ( PageContent.oneColumn [ viewLoading model.subPage ]
            , viewDescription serviceName [ Placeholder.view Placeholder.text ]
            )

        ( content, pageTitleDescription ) =
            case model.service of
                NotAsked ->
                    loading_

                Loading ->
                    loading_

                Success service ->
                    case model.subPage of
                        Activity activity ->
                            let
                                byAt =
                                    case service.latestDeploy of
                                        Just deploy ->
                                            ByAt.byAt deploy.deployedBy deploy.deployedAt
                                                |> ByAt.view appContext.timeZone appContext.now

                                        Nothing ->
                                            UI.nothing
                            in
                            ( PageContent.map ServiceActivityPageMsg
                                (ServiceActivityPage.view appContext serviceName activity)
                            , viewDescription serviceName [ byAt ]
                            )

                Failure e ->
                    ( PageContent.oneColumn [ viewError e ], viewDescription serviceName [] )

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
