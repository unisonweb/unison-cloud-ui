module UnisonCloud.AppHeader exposing (..)

import Html exposing (Html, div, h1, span, text)
import Html.Attributes exposing (class, classList)
import UI.ActionMenu as ActionMenu
import UI.AppHeader as AppHeader exposing (AppHeader)
import UI.Avatar as Avatar
import UI.Click as Click exposing (Click)
import UI.Icon as Icon
import UnisonCloud.Link as Link
import UnisonCloud.Session exposing (Session)


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


{-| Represents app level context, that is injected at render time
-}
type OpenedAppHeaderMenu
    = NoneOpened
    | HelpAndResourcesMenu
    | AccountMenu


type alias AppHeaderContext msg =
    { session : Session
    , openedAppHeaderMenu : OpenedAppHeaderMenu
    , toggleHelpAndResourcesMenuMsg : msg
    , toggleAccountMenuMsg : msg
    }


isHelpAndResourcesMenuOpen : OpenedAppHeaderMenu -> Bool
isHelpAndResourcesMenuOpen openedAppHeaderMenu =
    openedAppHeaderMenu == HelpAndResourcesMenu


isAccountMenuOpen : OpenedAppHeaderMenu -> Bool
isAccountMenuOpen openedAppHeaderMenu =
    openedAppHeaderMenu == AccountMenu


view : AppHeaderContext msg -> AppHeader msg -> Html msg
view ctx appHeader_ =
    let
        helpAndResources =
            ActionMenu.items
                (ActionMenu.optionItem Icon.docs "Cloud Docs" Link.cloudDocs)
                [ ActionMenu.optionItem Icon.unfoldedMap "Code of Conduct" Link.codeOfConduct
                , ActionMenu.optionItem Icon.unisonMark "Unison Website" Link.website
                , ActionMenu.optionItem Icon.github "Unison on GitHub" Link.github
                ]
                |> ActionMenu.fromButton ctx.toggleHelpAndResourcesMenuMsg "Help & Resources"
                |> ActionMenu.shouldBeOpen (isHelpAndResourcesMenuOpen ctx.openedAppHeaderMenu)
                |> ActionMenu.withButtonIcon Icon.questionmark
                |> ActionMenu.view
                |> (\hr -> div [ class "help-and-resources" ] [ hr ])

        avatar =
            Avatar.avatar ctx.session.avatarUrl (Maybe.map (String.left 1) ctx.session.name)
                |> Avatar.view

        viewAccountMenuTrigger isOpen =
            let
                chevron =
                    if isOpen then
                        Icon.chevronUp

                    else
                        Icon.chevronDown
            in
            div [ classList [ ( "account-menu-trigger", True ), ( "account-menu_is-open", isOpen ) ] ]
                [ avatar, Icon.view chevron ]

        accountMenu =
            ActionMenu.items
                (ActionMenu.optionItem Icon.creditCard "Manage Subscription" (Link.stripeCustomerPortal ctx.session))
                [ ActionMenu.optionItem Icon.exitDoor "Sign Out" Link.logout
                ]
                |> ActionMenu.fromCustom ctx.toggleAccountMenuMsg viewAccountMenuTrigger
                |> ActionMenu.shouldBeOpen (isAccountMenuOpen ctx.openedAppHeaderMenu)
                |> ActionMenu.view
                |> (\a -> div [ class "account-menu" ] [ a ])
    in
    appHeader_
        |> AppHeader.withRightSide [ helpAndResources, accountMenu ]
        |> AppHeader.view
