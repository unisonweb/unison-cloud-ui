module UnisonCloud.Page.ManageSubscriptionPage exposing (..)

import Html exposing (div, strong, text)
import Html.Attributes exposing (class)
import UI.Button as Button
import UI.Card as Card
import UI.Divider as Divider
import UI.Icon as Icon
import UI.PageContent as PageContent
import UI.PageLayout as PageLayout
import UI.PageTitle as PageTitle
import UnisonCloud.AppContext exposing (AppContext)
import UnisonCloud.AppDocument exposing (AppDocument)
import UnisonCloud.AppHeader as AppHeader
import UnisonCloud.CloudPlan as CloudPlan
import UnisonCloud.Link as Link
import UnisonCloud.PageFooter as PageFooter


view : AppContext -> AppDocument msg
view { session } =
    let
        managePlan =
            case session.cloudPlan of
                CloudPlan.Free ->
                    div [ class "upgrade" ]
                        [ text "Interested in the "
                        , Link.view_ (strong [] [ text "Starter" ]) Link.cloudPricing
                        , text " plan? "
                        , Button.button_ Link.starterSignup "Upgrade now!" |> Button.emphasized |> Button.view
                        ]

                _ ->
                    div [ class "manage-in-stripe" ]
                        [ text ("Manage your " ++ CloudPlan.toString session.cloudPlan ++ " plan via the ")
                        , Button.iconThenLabel_ (Link.stripeCustomerPortal session) Icon.creditCard "Stripe Customer Portal"
                            |> Button.emphasized
                            |> Button.view
                        ]

        content =
            [ Card.card
                [ div [ class "active-plan" ]
                    [ div [] [ text "Active plan:" ]
                    , strong [] [ text (CloudPlan.toString session.cloudPlan) ]
                    ]
                , Divider.divider |> Divider.small |> Divider.withoutMargin |> Divider.view
                , managePlan
                ]
                |> Card.asContained
                |> Card.view
            ]

        page =
            PageLayout.centeredLayout
                (PageContent.oneColumn content
                    |> PageContent.withPageTitle (PageTitle.title "Manage Subscription")
                )
                PageFooter.pageFooter
    in
    { pageId = "manage-subscription-page"
    , title = "Manage Subscription"
    , appHeader = AppHeader.appHeader
    , page = PageLayout.view page
    , modal = Nothing
    }
