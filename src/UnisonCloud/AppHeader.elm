module UnisonCloud.AppHeader exposing (appHeader)

import Html exposing (h1, text)
import UI.AppHeader as AppHeader exposing (AppHeader)
import UnisonCloud.Link as Link


appHeader : AppHeader msg
appHeader =
    AppHeader.appHeader (AppHeader.AppTitle Link.overview (h1 [] [ text "Unison Cloud" ]))
