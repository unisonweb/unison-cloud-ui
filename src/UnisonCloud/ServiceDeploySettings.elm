module UnisonCloud.ServiceDeploySettings exposing (..)

import Html exposing (Html, div, p, text)
import Html.Attributes exposing (class, classList)
import Http
import Lib.HttpApi as HttpApi exposing (HttpResult)
import Lib.Util as Util
import List.Nonempty as NEL
import UI
import UI.AnchoredOverlay as AnchoredOverlay exposing (AnchoredOverlay)
import UI.Button as Button
import UI.Click as Click
import UI.Form.RadioField as RadioField
import UI.Form.TextField as TextField
import UI.Icon as Icon
import UI.Modal as Modal
import UI.StatusBanner as StatusBanner
import UI.TabList as TabList
import UnisonCloud.Api as ShareApi
import UnisonCloud.AppContext exposing (AppContext)
import UnisonCloud.Service exposing (Service)
import UnisonCloud.Service.ServiceName as ServiceName exposing (ServiceName)
import UnisonCloud.ServiceHash as ServiceHash exposing (ServiceHash)



-- MODEL


type AssignToTab
    = NewService
    | ExistingService


type alias AssignToForm =
    -- Why aren't the input fields behind the AssignToTab sum type? Simply to
    -- allow users to switch tab while keeping their input.
    { newServiceName : String
    , selectedExistingServiceName : Maybe ServiceName
    , tab : AssignToTab
    }


type AssignTo
    = AssignForm AssignToForm
    | Assigning AssignToForm
    | Assigned AssignToForm
    | AssignFailure Http.Error AssignToForm


type Undeploy
    = NeedsConfirmation
    | Undeploying
    | Undeployed
    | UndeployFailure Http.Error


type Modal
    = NoModal
    | AssignToServiceModal ServiceHash AssignTo
    | UndeployConfirmationModal ServiceHash Undeploy


type Sheet
    = Closed
    | Open ServiceHash


type alias Model =
    { modal : Modal, sheet : Sheet }


init : Model
init =
    { modal = NoModal, sheet = Closed }



-- UPDATE


type Msg
    = OpenSheet ServiceHash
    | CloseSheet
    | ShowAssignModal
    | ChangeAssignToTab AssignToTab
    | SaveAssignTo
    | SaveAssignToFinished (HttpResult ())
    | UpdateServiceName String
    | UpdateSelectedExistingServiceName ServiceName
    | ShowUndeployConfirmationModal
    | UndeployConfirm
    | CloseModal
    | UndeployServiceDeployFinished (HttpResult ())


type OutMsg
    = None
    | UndeployedServiceDeploy ServiceHash
    | ShowAssignModalRequest ServiceHash
    | AssignedToService ServiceHash


update : AppContext -> List Service -> Msg -> Model -> ( Model, Cmd Msg, OutMsg )
update appContext existingServices msg model =
    case ( msg, model.sheet ) of
        ( OpenSheet serviceHash, Closed ) ->
            ( { model | sheet = Open serviceHash }, Cmd.none, None )

        ( CloseSheet, _ ) ->
            ( { model | sheet = Closed }, Cmd.none, None )

        ( CloseModal, _ ) ->
            ( { model | modal = NoModal }, Cmd.none, None )

        ( ShowAssignModal, Open serviceHash ) ->
            ( { model
                | modal =
                    AssignToServiceModal serviceHash
                        (AssignForm
                            { newServiceName = ""
                            , selectedExistingServiceName = existingServices |> List.head |> Maybe.map .name
                            , tab = NewService
                            }
                        )
                , sheet = Closed
              }
            , Cmd.none
            , None
            )

        ( ChangeAssignToTab newTab, _ ) ->
            updateAssignToForm (\f -> { f | tab = newTab }) model

        ( UpdateServiceName newName, _ ) ->
            updateAssignToForm (\f -> { f | newServiceName = newName }) model

        ( UpdateSelectedExistingServiceName serviceName, _ ) ->
            updateAssignToForm (\f -> { f | selectedExistingServiceName = Just serviceName }) model

        ( SaveAssignTo, _ ) ->
            case model.modal of
                AssignToServiceModal serviceHash assignTo ->
                    let
                        form =
                            assignToForm assignTo

                        newModel =
                            { model | modal = AssignToServiceModal serviceHash (Assigning form) }
                    in
                    case ( form.tab, form.selectedExistingServiceName ) of
                        ( NewService, _ ) ->
                            case ServiceName.fromString form.newServiceName of
                                Just serviceName ->
                                    ( newModel, assignToService appContext serviceName serviceHash, None )

                                _ ->
                                    ( model, Cmd.none, None )

                        ( ExistingService, Just name ) ->
                            ( newModel, assignToService appContext name serviceHash, None )

                        ( ExistingService, Nothing ) ->
                            ( model, Cmd.none, None )

                _ ->
                    ( model, Cmd.none, None )

        ( SaveAssignToFinished result, _ ) ->
            case model.modal of
                AssignToServiceModal serviceHash assignTo ->
                    case result of
                        Ok _ ->
                            ( { model | modal = AssignToServiceModal serviceHash (Assigned (assignToForm assignTo)) }
                            , Util.delayMsg 1500 CloseModal
                            , AssignedToService serviceHash
                            )

                        Err e ->
                            ( { model | modal = AssignToServiceModal serviceHash (AssignFailure e (assignToForm assignTo)) }
                            , Cmd.none
                            , None
                            )

                _ ->
                    ( model, Cmd.none, None )

        ( ShowUndeployConfirmationModal, Open serviceHash ) ->
            ( { model
                | modal = UndeployConfirmationModal serviceHash NeedsConfirmation
                , sheet = Closed
              }
            , Cmd.none
            , None
            )

        ( UndeployConfirm, _ ) ->
            case model.modal of
                UndeployConfirmationModal h _ ->
                    ( { model | modal = UndeployConfirmationModal h Undeploying }, undeployServiceDeploy appContext h, None )

                _ ->
                    ( model, Cmd.none, None )

        ( UndeployServiceDeployFinished r, _ ) ->
            case ( model.modal, r ) of
                ( UndeployConfirmationModal h _, Ok () ) ->
                    ( { model | modal = UndeployConfirmationModal h Undeployed }
                    , Util.delayMsg 1500 CloseModal
                    , UndeployedServiceDeploy h
                    )

                ( UndeployConfirmationModal h _, Err e ) ->
                    ( { model | modal = UndeployConfirmationModal h (UndeployFailure e) }, Cmd.none, None )

                _ ->
                    ( model, Cmd.none, None )

        _ ->
            ( model, Cmd.none, None )


updateAssignToForm : (AssignToForm -> AssignToForm) -> Model -> ( Model, Cmd Msg, OutMsg )
updateAssignToForm f model =
    let
        update_ serviceHash ctor form_ =
            ( { model
                | modal = AssignToServiceModal serviceHash (ctor (f form_))
              }
            , Cmd.none
            , None
            )
    in
    case model.modal of
        AssignToServiceModal serviceHash (AssignForm form) ->
            update_ serviceHash AssignForm form

        AssignToServiceModal serviceHash (AssignFailure e form) ->
            update_ serviceHash AssignForm form

        _ ->
            ( model, Cmd.none, None )


assignToForm : AssignTo -> AssignToForm
assignToForm assignTo =
    case assignTo of
        AssignForm f ->
            f

        Assigning f ->
            f

        Assigned f ->
            f

        AssignFailure _ f ->
            f



-- EFFECTS


assignToService : AppContext -> ServiceName -> ServiceHash -> Cmd Msg
assignToService appContext serviceName serviceHash =
    ShareApi.createServiceAssignment appContext.session.handle serviceName serviceHash
        |> HttpApi.toRequestWithEmptyResponse SaveAssignToFinished
        |> HttpApi.perform appContext.api


undeployServiceDeploy : AppContext -> ServiceHash -> Cmd Msg
undeployServiceDeploy appContext serviceHash =
    ShareApi.undeployServiceDeploy serviceHash
        |> HttpApi.toRequestWithEmptyResponse UndeployServiceDeployFinished
        |> HttpApi.perform appContext.api



-- VIEW MODAL


viewAssignToServiceModal : List Service -> AssignTo -> Modal.Modal Msg
viewAssignToServiceModal existingServices assignTo =
    let
        ( form, dimOverlay, status ) =
            case assignTo of
                AssignForm f ->
                    ( f, False, UI.nothing )

                Assigning f ->
                    ( f, True, StatusBanner.working "Saving..." )

                Assigned f ->
                    ( f, True, StatusBanner.good "Successfully saved" )

                AssignFailure _ f ->
                    ( f, False, StatusBanner.bad "Couldn't save" )

        newServiceContent =
            [ TextField.fieldWithoutLabel
                UpdateServiceName
                "Service Name"
                form.newServiceName
                |> TextField.withHelpText "Must exist of letters, numbers, or dashes. No spaces, and no other symbols."
                |> TextField.withIsValid ServiceName.isValidName
                |> TextField.withAutofocus
                |> TextField.view
            ]

        options =
            existingServices
                |> List.map (\s -> RadioField.option_ (ServiceName.toString s.name) s.name)
                |> NEL.fromList

        existingServiceTabContent =
            case ( form.selectedExistingServiceName, options ) of
                ( Just selected, Just options_ ) ->
                    [ RadioField.field
                        "Choose a service"
                        UpdateSelectedExistingServiceName
                        options_
                        selected
                        |> RadioField.view
                    ]

                _ ->
                    []

        tabs =
            { newService =
                TabList.tab "New Service"
                    (Click.onClick (ChangeAssignToTab NewService))
            , existingService =
                TabList.tab "Existing Service"
                    (Click.onClick (ChangeAssignToTab ExistingService))
            }

        ( tabList, tabContent ) =
            case form.tab of
                NewService ->
                    ( TabList.tabList []
                        tabs.newService
                        [ tabs.existingService ]
                    , newServiceContent
                    )

                ExistingService ->
                    ( TabList.tabList [ tabs.newService ] tabs.existingService []
                    , existingServiceTabContent
                    )

        content =
            div
                [ class "assign-to-service-modal" ]
                [ p [] [ text "Assigning service deployments to a name allows them to get a stable URL." ]
                , TabList.view tabList
                , div [ class "tab-content" ] tabContent
                ]
    in
    content
        |> Modal.content
        |> Modal.modal "assign-to-service-modal" CloseModal
        |> Modal.withAttributes
            [ classList
                [ ( "assign-to-service-modal_dim-overlay", dimOverlay ) ]
            ]
        |> Modal.withHeader "Assign deployment to a service"
        |> Modal.withLeftSideFooter [ status ]
        |> Modal.withActions
            [ Button.button CloseModal "Cancel"
                |> Button.subdued
            , Button.button SaveAssignTo "Save"
                |> Button.emphasized
            ]


viewUndeployConfirmationModal : ServiceHash -> Undeploy -> Modal.Modal Msg
viewUndeployConfirmationModal serviceHash undeploy =
    let
        ( leftSide, dimOverlay ) =
            case undeploy of
                NeedsConfirmation ->
                    ( UI.nothing, False )

                Undeploying ->
                    ( StatusBanner.working "Undeploying..", True )

                Undeployed ->
                    ( StatusBanner.good "Successfully undeployed", True )

                UndeployFailure _ ->
                    ( StatusBanner.bad
                        "Something broke on our end and we couldn't undeploy. Please try again."
                    , False
                    )

        content =
            div [ class "undeploy-confirmation-modal" ]
                [ p []
                    [ text "Undeploying this service ("
                    , text (ServiceHash.toShortString serviceHash)
                    , text ") will render it unreachable."
                    ]
                ]
    in
    content
        |> Modal.content
        |> Modal.modal "undeploy-confirmation-modal" CloseModal
        |> Modal.withAttributes
            [ classList
                [ ( "undeploy-confirmation-modal_dim-overlay", dimOverlay ) ]
            ]
        |> Modal.withHeader "Are you sure you want to undeploy?"
        |> Modal.withActions
            [ Button.button CloseModal "Cancel"
                |> Button.subdued
            , Button.button UndeployConfirm "Yes, undeploy it"
                |> Button.critical
            ]
        |> Modal.withLeftSideFooter [ leftSide ]


viewModal : List Service -> Model -> Maybe (Modal.Modal Msg)
viewModal existingServices model =
    case model.modal of
        NoModal ->
            Nothing

        AssignToServiceModal _ assignTo ->
            Just (viewAssignToServiceModal existingServices assignTo)

        UndeployConfirmationModal serviceHash undeploy ->
            Just (viewUndeployConfirmationModal serviceHash undeploy)



-- VIEW MENU & SHEET


viewSheet : Html Msg
viewSheet =
    let
        assignOption =
            Click.view [ class "option assign assign-option" ]
                [ Icon.view Icon.writingPad, text "Assign to Service" ]
                (Click.onClick ShowAssignModal)

        undeployOption =
            Click.view [ class "option undeploy undeploy-option" ]
                [ Icon.view Icon.trash, text "Undeploy" ]
                (Click.onClick ShowUndeployConfirmationModal)
    in
    div [ class "service-deploy-settings_sheet" ]
        [ assignOption, undeployOption ]


toAnchoredOverlay : ServiceHash -> Model -> AnchoredOverlay Msg
toAnchoredOverlay serviceHash model =
    let
        ( toggleMsg, active ) =
            case model.sheet of
                Closed ->
                    ( OpenSheet serviceHash, False )

                Open sh ->
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
    case model.sheet of
        Closed ->
            ao_ button

        Open sh ->
            if ServiceHash.equals sh serviceHash then
                ao_ button
                    |> AnchoredOverlay.withSheetPosition AnchoredOverlay.BottomRight
                    |> AnchoredOverlay.withSheet (AnchoredOverlay.sheet viewSheet)

            else
                ao_ button


viewMenu : ServiceHash -> Model -> Html Msg
viewMenu serviceHash model =
    AnchoredOverlay.view (toAnchoredOverlay serviceHash model)
