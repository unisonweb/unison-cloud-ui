module UnisonCloud.Page.ServicePage.ServiceActivityPage exposing (..)

import Html exposing (Html)
import UI.PageContent as PageContent exposing (PageContent)
import UnisonCloud.AppContext exposing (AppContext)
import UnisonCloud.Log as Log
import UnisonCloud.Service.ServiceId exposing (ServiceId)



-- MODEL


type alias Model =
    { log : Log.Model
    }


init : AppContext -> ServiceId -> ( Model, Cmd Msg )
init appContext serviceId =
    let
        ( log, logCmd ) =
            Log.init appContext (Log.ServiceContext serviceId)
    in
    ( { log = log }
    , Cmd.map LogMsg logCmd
    )



-- UPDATE


type Msg
    = LogMsg Log.Msg


update : AppContext -> ServiceId -> Msg -> Model -> ( Model, Cmd Msg )
update appContext serviceId msg model =
    case msg of
        LogMsg logMsg ->
            let
                ( log, logCmd ) =
                    Log.update appContext
                        (Log.ServiceContext serviceId)
                        logMsg
                        model.log
            in
            ( { model | log = log }, Cmd.map LogMsg logCmd )



-- VIEW


viewLoading : Html msg
viewLoading =
    Log.viewLoading


view : AppContext -> ServiceId -> Model -> PageContent Msg
view appContext _ model =
    let
        log =
            Log.view appContext model.log
    in
    PageContent.oneColumn [ Html.map LogMsg log ]
