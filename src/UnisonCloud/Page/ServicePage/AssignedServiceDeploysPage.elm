module UnisonCloud.Page.ServicePage.AssignedServiceDeploysPage exposing (..)

import Html exposing (Html, div, text)
import Html.Attributes exposing (class)
import Http
import Json.Decode as Decode
import Lib.HttpApi as HttpApi
import Lib.Util as Util
import RemoteData exposing (RemoteData(..), WebData)
import UI.ByAt as ByAt
import UI.Card as Card
import UI.DateTime as DateTime
import UI.ErrorCard as ErrorCard
import UI.PageContent as PageContent exposing (PageContent)
import UI.Placeholder as Placeholder
import UnisonCloud.Api as CloudApi
import UnisonCloud.AppContext exposing (AppContext)
import UnisonCloud.Service.ServiceName exposing (ServiceName)
import UnisonCloud.ServiceDeploy as ServiceDeploy exposing (ServiceDeploy)
import UnisonCloud.ServiceHash as ServiceHash



-- MODEL


type alias Model =
    { deploys : WebData (List ServiceDeploy)
    }


init : AppContext -> ServiceName -> ( Model, Cmd Msg )
init appContext serviceName =
    ( { deploys = Loading }
    , fetchServiceDeploys appContext serviceName
    )



-- UPDATE


type Msg
    = FetchServiceDeploysFinished (WebData (List ServiceDeploy))


update : AppContext -> ServiceName -> Msg -> Model -> ( Model, Cmd Msg )
update _ _ msg model =
    case msg of
        FetchServiceDeploysFinished deploys ->
            let
                deploys_ =
                    deploys
                        |> RemoteData.map
                            (Util.sortByWith
                                (.deployedAt >> DateTime.toISO8601)
                                (Util.descending compare)
                            )
            in
            ( { model | deploys = deploys_ }, Cmd.none )



-- EFFECTS


fetchServiceDeploys : AppContext -> ServiceName -> Cmd Msg
fetchServiceDeploys appContext serviceName =
    CloudApi.assignedServiceDeploys serviceName
        |> HttpApi.toRequest
            (Decode.list ServiceDeploy.decode)
            (RemoteData.fromResult >> FetchServiceDeploysFinished)
        |> HttpApi.perform appContext.api



-- VIEW


viewLoading : Html msg
viewLoading =
    let
        placeholder_ length intensity =
            Placeholder.text |> Placeholder.withLength length |> Placeholder.withIntensity intensity |> Placeholder.view

        placeholders =
            [ placeholder_ Placeholder.Medium Placeholder.Normal
            , placeholder_ Placeholder.Small Placeholder.Subdued
            , placeholder_ Placeholder.Large Placeholder.Subdued
            , placeholder_ Placeholder.Medium Placeholder.Subdued
            , placeholder_ Placeholder.Medium Placeholder.Normal
            , placeholder_ Placeholder.Small Placeholder.Subdued
            , placeholder_ Placeholder.Large Placeholder.Subdued
            , placeholder_ Placeholder.Medium Placeholder.Subdued
            ]
    in
    Card.card placeholders
        |> Card.view


viewError : Http.Error -> Html msg
viewError _ =
    ErrorCard.errorCard
        "Couldn't load deploys"
        "Something unexpected happened on our end when loading the deploys and we can't display them."
        |> ErrorCard.toCard
        |> Card.view


viewDeploy : AppContext -> ServiceDeploy -> Html msg
viewDeploy appContext deploy =
    let
        byAt =
            ByAt.byAt deploy.deployedBy deploy.deployedAt
    in
    div [ class "service-deploys-page_deploy" ]
        [ text (ServiceHash.toShortString deploy.hash)
        , ByAt.view appContext.timeZone appContext.now byAt
        ]


viewDeploys : AppContext -> List ServiceDeploy -> Html msg
viewDeploys appContext deploys =
    div [ class "service-deploys-page_deploys" ] (List.map (viewDeploy appContext) deploys)


view : AppContext -> ServiceName -> Model -> PageContent Msg
view appContext _ model =
    let
        content =
            case model.deploys of
                NotAsked ->
                    viewLoading

                Loading ->
                    viewLoading

                Success deploys ->
                    viewDeploys appContext deploys

                Failure e ->
                    viewError e
    in
    PageContent.oneColumn [ content ]
