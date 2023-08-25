module UnisonCloud.Page.ServicePage.ServiceDeploysPage exposing (..)

import Html exposing (Html, text)
import Http
import Json.Decode as Decode
import Lib.HttpApi as HttpApi
import RemoteData exposing (RemoteData(..), WebData)
import UI.Card as Card
import UI.ErrorCard as ErrorCard
import UI.PageContent as PageContent exposing (PageContent)
import UnisonCloud.Api as CloudApi
import UnisonCloud.AppContext exposing (AppContext)
import UnisonCloud.Log as Log
import UnisonCloud.Service.ServiceName exposing (ServiceName)
import UnisonCloud.ServiceDeploy as ServiceDeploy exposing (ServiceDeploy)



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
            ( { model | deploys = deploys }, Cmd.none )



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
    Log.viewLoading


viewError : Http.Error -> Html msg
viewError _ =
    ErrorCard.errorCard
        "Couldn't load deploys"
        "Something unexpected happened on our end when loading the deploys and we can't display them."
        |> ErrorCard.toCard
        |> Card.view


view : AppContext -> ServiceName -> Model -> PageContent Msg
view _ _ _ =
    PageContent.oneColumn [ text "TODO" ]
