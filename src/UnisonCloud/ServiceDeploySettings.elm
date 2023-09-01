module UnisonCloud.ServiceDeploySettings exposing (..)

import Html exposing (Html, div, text)
import Html.Attributes exposing (class)
import Http
import Lib.HttpApi as HttpApi exposing (HttpResult)
import UI
import UI.AnchoredOverlay as AnchoredOverlay exposing (AnchoredOverlay)
import UI.Button as Button
import UI.Click as Click
import UI.Icon as Icon
import UI.Placeholder as Placeholder
import UnisonCloud.Api as ShareApi
import UnisonCloud.AppContext exposing (AppContext)
import UnisonCloud.ServiceHash as ServiceHash exposing (ServiceHash)



-- MODEL


type Undeploy
    = Idle
    | NeedsConfirmation
    | Undeploying
    | Success
    | Failure Http.Error


type alias Sheet =
    { undeploy : Undeploy
    }


type Model
    = Closed
    | Open ServiceHash Sheet


init : Model
init =
    Closed



-- UPDATE


type Msg
    = OpenSheet ServiceHash
    | CloseSheet
    | ShowUndeployServiceDeployConfirm
    | UndeployServiceDeployConfirm
    | UndeployServiceDeployCancel
    | UndeployServiceDeployFinished (HttpResult ())


type OutMsg
    = None
    | UndeployedServiceDeploy ServiceHash


update : AppContext -> Msg -> Model -> ( Model, Cmd Msg, OutMsg )
update appContext msg model =
    case ( msg, model ) of
        ( OpenSheet serviceHash, Closed ) ->
            let
                sheet =
                    { undeploy = Idle }
            in
            ( Open serviceHash sheet
            , Cmd.none
            , None
            )

        ( CloseSheet, _ ) ->
            ( Closed, Cmd.none, None )

        ( ShowUndeployServiceDeployConfirm, Open h s ) ->
            ( Open h { s | undeploy = NeedsConfirmation }, Cmd.none, None )

        ( UndeployServiceDeployCancel, Open h s ) ->
            ( Open h { s | undeploy = Idle }, Cmd.none, None )

        ( UndeployServiceDeployConfirm, Open h s ) ->
            ( Open h { s | undeploy = Undeploying }, undeployServiceDeploy appContext h, None )

        ( UndeployServiceDeployFinished r, Open h s ) ->
            case r of
                Ok () ->
                    ( Closed, Cmd.none, UndeployedServiceDeploy h )

                Err e ->
                    ( Open h { s | undeploy = Failure e }, Cmd.none, None )

        _ ->
            ( model, Cmd.none, None )



-- EFFECTS


undeployServiceDeploy : AppContext -> ServiceHash -> Cmd Msg
undeployServiceDeploy appContext serviceHash =
    ShareApi.undeployServiceDeploy serviceHash
        |> HttpApi.toRequestWithEmptyResponse UndeployServiceDeployFinished
        |> HttpApi.perform appContext.api



-- VIEW


viewUndeploy : Undeploy -> Html Msg
viewUndeploy undeploy =
    case undeploy of
        Idle ->
            Click.view [ class "undeploy undeploy-option" ]
                [ Icon.view Icon.trash, text "Undeploy" ]
                (Click.onClick ShowUndeployServiceDeployConfirm)

        NeedsConfirmation ->
            div [ class "undeploy undeploy-confirm" ]
                [ text "Are you sure?"
                , div [ class "undeploy-confirm-actions" ]
                    [ Button.button UndeployServiceDeployCancel "Cancel"
                        |> Button.small
                        |> Button.view
                    , Button.button UndeployServiceDeployConfirm "Yes, undeploy it"
                        |> Button.small
                        |> Button.critical
                        |> Button.view
                    ]
                ]

        Undeploying ->
            div [ class "undeploy undeploying" ]
                [ Placeholder.text
                    |> Placeholder.withLength Placeholder.Large
                    |> Placeholder.view
                ]

        Success ->
            UI.nothing

        Failure _ ->
            div [ class "undeploy undeploy-failure" ] [ text "Undeploy failed." ]


viewSheet : Sheet -> Html Msg
viewSheet sheet =
    div [ class "service-deploy-settings_sheet" ] [ viewUndeploy sheet.undeploy ]


toAnchoredOverlay : ServiceHash -> Model -> AnchoredOverlay Msg
toAnchoredOverlay serviceHash model =
    let
        ( toggleMsg, active ) =
            case model of
                Closed ->
                    ( OpenSheet serviceHash, False )

                Open sh _ ->
                    if ServiceHash.equals sh serviceHash then
                        ( CloseSheet, True )

                    else
                        ( OpenSheet serviceHash, False )

        button =
            Button.icon toggleMsg Icon.cog
                |> Button.small
                |> Button.subdued
                |> Button.withIsActive active
                |> Button.view

        ao_ =
            AnchoredOverlay.anchoredOverlay CloseSheet
    in
    case model of
        Closed ->
            ao_ button

        Open sh s ->
            if ServiceHash.equals sh serviceHash then
                ao_ button
                    |> AnchoredOverlay.withSheetPosition AnchoredOverlay.BottomRight
                    |> AnchoredOverlay.withSheet (AnchoredOverlay.sheet (viewSheet s))

            else
                ao_ button


view : ServiceHash -> Model -> Html Msg
view serviceHash model =
    AnchoredOverlay.view (toAnchoredOverlay serviceHash model)
