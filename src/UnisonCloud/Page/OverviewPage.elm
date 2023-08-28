module UnisonCloud.Page.OverviewPage exposing (..)

import Html exposing (text)
import UI.PageContent as PageContent
import UI.PageLayout as PageLayout
import UnisonCloud.AppDocument exposing (AppDocument)
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
    , title = "Overview | Unison Cloud"
    , appHeader = Appheader.appHeader
    , page = PageLayout.view page
    , modal = Nothing
    }
