module UnisonCloud.AppDocument exposing
    ( AppDocument
    , appDocument
    , map
    , view
    , withModal
    , withModal_
    )

import Browser exposing (Document)
import Html exposing (Html, div)
import Html.Attributes exposing (class, id)
import Maybe.Extra as MaybeE
import UI
import UI.AppHeader exposing (AppHeader)
import UnisonCloud.AppHeader as AppHeader exposing (AppHeaderContext)



{-

   AppDocument
   ===========

   Very similar to Browser.Document, but includes a common app title and app
   frame, as well as slots for header, page, and modals.
-}


type alias AppDocument msg =
    { pageId : String
    , title : String
    , appHeader : AppHeader msg
    , page : Html msg
    , modal : Maybe (Html msg)
    }



-- CREATE


appDocument : String -> String -> AppHeader msg -> Html msg -> AppDocument msg
appDocument pageId title appHeader page =
    { pageId = pageId
    , title = title
    , appHeader = appHeader
    , page = page
    , modal = Nothing
    }



-- MODIFY


withModal : Html msg -> AppDocument msg -> AppDocument msg
withModal modal appDoc =
    withModal_ (Just modal) appDoc


withModal_ : Maybe (Html msg) -> AppDocument msg -> AppDocument msg
withModal_ modal appDoc =
    { appDoc | modal = modal }



-- MAP


map : (msgA -> msgB) -> AppDocument msgA -> AppDocument msgB
map toMsgB { pageId, title, appHeader, page, modal } =
    { pageId = pageId
    , title = title
    , appHeader = UI.AppHeader.map toMsgB appHeader
    , page = Html.map toMsgB page
    , modal = Maybe.map (Html.map toMsgB) modal
    }



-- VIEW


viewAnnouncement : Html msg -> Html msg
viewAnnouncement content =
    div [ id "announcement" ] [ content ]


view : AppHeaderContext msg -> AppDocument msg -> List (Html msg) -> Document msg
view appHeaderCtx { pageId, title, appHeader, page, modal } extra =
    let
        announcement =
            Nothing
    in
    { title = title ++ " | Unison Cloud"
    , body =
        div
            [ id "app"
            , class pageId
            ]
            [ MaybeE.unwrap UI.nothing viewAnnouncement announcement
            , AppHeader.view appHeaderCtx appHeader
            , page
            , Maybe.withDefault UI.nothing modal
            ]
            :: extra
    }
