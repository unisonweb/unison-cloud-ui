module UnisonCloud.Page.ServicePage.ServiceActivityPage exposing (..)

import Html exposing (Html)
import Http
import Lib.HttpApi as HttpApi
import RemoteData exposing (RemoteData(..), WebData)
import UI.Card as Card
import UI.ErrorCard as ErrorCard
import UI.PageContent as PageContent exposing (PageContent)
import UnisonCloud.Api as CloudApi
import UnisonCloud.AppContext exposing (AppContext)
import UnisonCloud.Log as Log
import UnisonCloud.Service as Service exposing (Service)
import UnisonCloud.Service.ServiceName exposing (ServiceName)



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
        "Couldn't load activity"
        "Something unexpected happened on our end when loading the service activity and we can't display it."
        |> ErrorCard.toCard
        |> Card.view


view : AppContext -> ServiceName -> Model -> PageContent Msg
view appContext _ model =
    let
        log =
            Log.view appContext model.log
    in
    PageContent.oneColumn [ Html.map LogMsg log ]
