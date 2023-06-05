module UnisonCloud.Page.OverviewPage exposing (..)

import Html exposing (text)
import UI.AppDocument exposing (AppDocument)
import UI.PageContent as PageContent
import UI.PageLayout as PageLayout
import UnisonCloud.AppHeader as Appheader


view : AppDocument msg
view =
    let
        page =
            PageLayout.centeredLayout
                (PageContent.oneColumn [ text "Welcome to Unison Cloud" ])
                (PageLayout.PageFooter [])
    in
    { pageId = "overview-page"
    , title = "Unison Cloud | Overview"
    , announcement = Nothing
    , appHeader = Appheader.appHeader
    , pageHeader = Nothing
    , page = PageLayout.view page
    , modal = Nothing
    }
