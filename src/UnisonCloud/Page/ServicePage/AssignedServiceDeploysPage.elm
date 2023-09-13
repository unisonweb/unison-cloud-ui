module UnisonCloud.Page.ServicePage.AssignedServiceDeploysPage exposing (..)

import Html exposing (Html, div, text)
import Html.Attributes exposing (class)
import Http
import Json.Decode as Decode
import Lib.HttpApi as HttpApi
import Lib.Util as Util
import RemoteData exposing (RemoteData(..), WebData)
import UI
import UI.ByAt as ByAt
import UI.Card as Card
import UI.Click as Click
import UI.DateTime as DateTime
import UI.ErrorCard as ErrorCard
import UI.ExternalLinkIcon as ExternalLinkIcon
import UI.Icon as Icon
import UI.PageContent as PageContent exposing (PageContent)
import UI.Placeholder as Placeholder
import UI.Tag as Tag
import UnisonCloud.Api as CloudApi
import UnisonCloud.AppContext exposing (AppContext)
import UnisonCloud.Link as Link
import UnisonCloud.Service as Service exposing (Service)
import UnisonCloud.Service.ServiceId exposing (ServiceId)
import UnisonCloud.ServiceDeploy as ServiceDeploy exposing (ServiceDeploySummary)
import UnisonCloud.ServiceHash as ServiceHash
import Url



-- MODEL


type alias Model =
    { deploys : WebData (List ServiceDeploySummary)
    }


init : AppContext -> ServiceId -> ( Model, Cmd Msg )
init appContext serviceId =
    ( { deploys = Loading }
    , fetchServiceDeploys appContext serviceId
    )



-- UPDATE


type Msg
    = FetchServiceDeploysFinished (WebData (List ServiceDeploySummary))


update : AppContext -> ServiceId -> Msg -> Model -> ( Model, Cmd Msg )
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


fetchServiceDeploys : AppContext -> ServiceId -> Cmd Msg
fetchServiceDeploys appContext serviceId =
    CloudApi.assignedServiceDeploys serviceId
        |> HttpApi.toRequest
            (Decode.list ServiceDeploy.decodeSummary)
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


viewDeploy : AppContext -> Service -> ServiceDeploySummary -> Html msg
viewDeploy appContext service deploy =
    let
        exposedLink =
            case ServiceDeploy.exposedUrl appContext deploy of
                Just url ->
                    ExternalLinkIcon.view
                        (Click.externalHref (Url.toString url))

                Nothing ->
                    UI.nothing

        activeTag =
            if Service.isActiveDeploy service deploy then
                Tag.tag "Active Deploy"
                    |> Tag.withIcon Icon.bolt
                    |> Tag.view

            else
                UI.nothing

        byAt =
            ByAt.byAt deploy.deployedBy deploy.deployedAt
    in
    div [ class "assigned-service-deploys-page_deploy" ]
        [ div [ class "assigned-service-deploy_hash" ]
            [ Click.view []
                [ text (ServiceHash.toShortString deploy.hash) ]
                (Link.serviceDeployForService service.id deploy.hash)
            , exposedLink
            ]
        , activeTag
        , ByAt.view appContext.timeZone appContext.now byAt
        ]


viewDeploys : AppContext -> Service -> List ServiceDeploySummary -> Html msg
viewDeploys appContext service deploys =
    Card.card
        (List.map (viewDeploy appContext service) deploys)
        |> Card.withClassName "assigned-service-deploys-page_deploys"
        |> Card.asContained
        |> Card.view


view : AppContext -> Service -> Model -> PageContent Msg
view appContext service model =
    let
        content =
            case model.deploys of
                NotAsked ->
                    viewLoading

                Loading ->
                    viewLoading

                Success deploys ->
                    viewDeploys appContext service deploys

                Failure e ->
                    viewError e
    in
    PageContent.oneColumn [ content ]
