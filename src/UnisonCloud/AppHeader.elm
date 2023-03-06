module UnisonCloud.AppHeader exposing (..)

import Html exposing (Html, h1, span, text)
import Html.Attributes exposing (class)
import UI.AppHeader as AppHeader exposing (AppHeader)
import UI.Click as Click exposing (Click)
import UnisonCloud.Link as Link


appTitle : AppHeader.AppTitle msg
appTitle =
    appTitle_ Link.overview


appTitle_ : Click msg -> AppHeader.AppTitle msg
appTitle_ click =
    AppHeader.AppTitle
        click
        (h1 []
            [ text "Unison"
            , span [ class "context unison-cloud" ] [ text "Cloud" ]
            ]
        )


blank : AppHeader msg
blank =
    AppHeader.appHeader (appTitle_ Click.disabled)


viewBlank : Html msg
viewBlank =
    AppHeader.view blank


appHeader : AppHeader msg
appHeader =
    AppHeader.appHeader appTitle
