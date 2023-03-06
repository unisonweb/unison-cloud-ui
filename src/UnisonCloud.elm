module UnisonCloud exposing (..)

import Browser
import UnisonCloud.App as App
import UnisonCloud.Env exposing (Flags)
import UnisonCloud.PreApp as PreApp


main : Program Flags PreApp.Model PreApp.Msg
main =
    Browser.application
        { init = PreApp.init
        , update = PreApp.update
        , view = PreApp.view
        , subscriptions = PreApp.subscriptions
        , onUrlRequest = App.LinkClicked >> PreApp.AppMsg
        , onUrlChange = App.UrlChanged >> PreApp.AppMsg
        }
