module UnisonCloud.Page.ErrorPage exposing (..)

import Html exposing (br, p, text)
import Html.Attributes exposing (class)
import UI.AppDocument exposing (AppDocument)
import UI.Button as Button
import UI.Card as Card
import UI.Icon as Icon
import UI.PageContent as PageContent
import UI.PageLayout as PageLayout
import UI.StatusMessage as StatusMessage
import UnisonCloud.AppError exposing (AppError(..))
import UnisonCloud.AppHeader as AppHeader
import UnisonCloud.Link as Link
import UnisonCloud.PageFooter as PageFooter


view : AppError -> AppDocument msg
view appError =
    let
        card =
            case appError of
                SignInNoCloudAccount ->
                    StatusMessage.bad "Not enabled for Unison Cloud"
                        [ p []
                            -- TODO: Some CTA to get on the wait list, link to form?
                            [ text "Your account is not yet enabled for Unison Cloud."
                            ]
                        ]
                        |> StatusMessage.withCta (Button.iconThenLabel_ Link.login Icon.github "Create Account with GitHub" |> Button.medium)
                        |> StatusMessage.asCard

                UnspecifiedError ->
                    StatusMessage.bad "Something went wrong 😞"
                        [ p [] [ text "Unfortunately, we couldn't successfully complete your request." ]
                        , p [ class "subtle" ]
                            [ text "The Unison team have been notified about this error,"
                            , br [] []
                            , text " so that we can help prevent it in the future."
                            ]
                        ]
                        |> StatusMessage.withCta (Button.iconThenLabel_ Link.reportBug Icon.bug "Report a Bug" |> Button.medium)
                        |> StatusMessage.asCard

        page =
            PageLayout.centeredLayout
                (PageContent.oneColumn [ Card.view card ])
                PageFooter.pageFooter
    in
    { pageId = "error-page"
    , title = "Something went wrong 😞"
    , announcement = Nothing
    , appHeader = AppHeader.appHeader
    , pageHeader = Nothing
    , page = PageLayout.view page
    , modal = Nothing
    }
