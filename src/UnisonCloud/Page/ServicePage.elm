module UnisonCloud.Page.ServicePage exposing (..)

import Html exposing (Html, text)
import Http
import UI.AppDocument exposing (AppDocument)
import UI.Card as Card
import UI.ErrorCard as ErrorCard
import UI.PageContent as PageContent
import UI.PageLayout as PageLayout
import UI.PageTitle as PageTitle
import UI.Placeholder as Placeholder
import UnisonCloud.AppContext exposing (AppContext)
import UnisonCloud.AppHeader as Appheader
import UnisonCloud.Service as Service exposing (ServiceId)



-- MODEL


type alias Model =
    ()


init : AppContext -> ServiceId -> ( Model, Cmd Msg )
init _ _ =
    ( (), Cmd.none )



-- UPDATE


type Msg
    = NoOp


update : AppContext -> ServiceId -> Msg -> Model -> ( Model, Cmd Msg )
update _ _ _ model =
    ( model, Cmd.none )



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

        viewCard_ =
            Card.card placeholders
                |> Card.asContained
                |> Card.view
    in
    viewCard_


viewError : Http.Error -> Html msg
viewError _ =
    ErrorCard.errorCard
        "Couldn't load service deploy"
        "Something unexpected happened on our end when loading the service deploy and we can't display it."
        |> ErrorCard.toCard
        |> Card.asContainedWithFade
        |> Card.view


view : AppContext -> ServiceId -> Model -> AppDocument Msg
view _ sid _ =
    let
        content =
            [ text "TODO" ]

        page =
            PageLayout.centeredLayout
                (PageContent.oneColumn content
                    |> PageContent.withPageTitle (PageTitle.title ("Service: " ++ Service.serviceIdToString sid))
                )
                (PageLayout.PageFooter [])
                |> PageLayout.withSubduedBackground
    in
    { pageId = "service-page"
    , title = "Service: " ++ Service.serviceIdToString sid ++ " | Unison Cloud"
    , announcement = Nothing
    , appHeader = Appheader.appHeader
    , pageHeader = Nothing
    , page = PageLayout.view page
    , modal = Nothing
    }
