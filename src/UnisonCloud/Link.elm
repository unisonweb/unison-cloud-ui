{-

   Link
   ====

   Various UI.Click link helpers for Routes and external links

-}


module UnisonCloud.Link exposing (..)

import Html exposing (Html, text)
import Lib.HttpApi as HttpApi exposing (HttpApi)
import UI.Click as Click exposing (Click)
import UnisonCloud.Account exposing (Account)
import UnisonCloud.Route as Route exposing (Route)
import UnisonCloud.Service.ServiceName exposing (ServiceName)
import UnisonCloud.ServiceHash exposing (ServiceHash)
import Url exposing (Url)



-- EXTERNAL URLS


link : String -> Click msg
link url =
    Click.externalHref url


unisonCloudWebsite : Click msg
unisonCloudWebsite =
    Click.externalHref "https://unison.cloud"


cloudPricing : Click msg
cloudPricing =
    Click.externalHref "https://unison.cloud/#pricing"


starterSignup : Click msg
starterSignup =
    Click.externalHref "https://unison.cloud/signup?plan=Starter"


website : Click msg
website =
    Click.externalHref "https://unison-lang.org"


whatsNew : Click msg
whatsNew =
    Click.externalHref "https://unison-lang.org/whats-new"


whatsNewPost : String -> Click msg
whatsNewPost postPath =
    Click.externalHref ("https://unison-lang.org/whats-new/" ++ postPath)


github : Click msg
github =
    Click.externalHref "https://github.com/unisonweb/unison"


githubReleases : Click msg
githubReleases =
    Click.externalHref "https://github.com/unisonweb/unison/releases"


githubRelease : String -> Click msg
githubRelease releaseTag =
    Click.externalHref ("https://github.com/unisonweb/unison/releases/tag/" ++ releaseTag)


reportBug : Click msg
reportBug =
    Click.externalHref "https://github.com/unisonweb/unison/issues/new?%5B%5Dlabels=unison-share"


docs : Click msg
docs =
    Click.externalHref "https://unison-lang.org/docs"


cloudDocs : Click msg
cloudDocs =
    Click.externalHref "https://share.unison-lang.org/@unison/cloud"


cloudStartDocs : Click msg
cloudStartDocs =
    Click.externalHref "https://share.unison-lang.org/@unison/cloud-start"


tour : Click msg
tour =
    Click.externalHref "https://unison-lang.org/docs/tour"


codeOfConduct : Click msg
codeOfConduct =
    Click.externalHref "https://www.unison-lang.org/community/code-of-conduct/"


status : Click msg
status =
    Click.externalHref "https://unison.statuspage.io"


discord : Click msg
discord =
    Click.externalHref "https://unison-lang.com/discord"


stripeCustomerPortal : Account a -> Click msg
stripeCustomerPortal account =
    Click.externalHref
        ("https://billing.stripe.com/p/login/fZe7wj02ZdXC3AIfYY?prefilled_email="
            ++ Url.percentEncode account.primaryEmail
        )


login : HttpApi -> Url -> Click msg
login api returnTo =
    let
        returnTo_ =
            returnTo
                |> Url.toString
                |> Url.percentEncode

        base =
            api
                |> HttpApi.baseApiUrl
                |> String.replace "/v2/" "/"
    in
    Click.externalHref (base ++ "login?return_to=" ++ returnTo_)


logout : HttpApi -> Url -> Click msg
logout api returnTo =
    let
        returnTo_ =
            returnTo
                |> Url.toString
                |> Url.percentEncode

        base =
            api
                |> HttpApi.baseApiUrl
                |> String.replace "/v2/" "/"
    in
    Click.externalHref (base ++ "logout?return_to=" ++ returnTo_)



-- INTERNAL URLS


overview : Click msg
overview =
    toClick Route.overview


manageSubscription : Click msg
manageSubscription =
    toClick Route.manageSubscription


services : Click msg
services =
    toClick Route.services


service : ServiceName -> Click msg
service sName =
    toClick (Route.service sName)


serviceActivity : ServiceName -> Click msg
serviceActivity sName =
    toClick (Route.serviceActivity sName)


serviceDeploysForService : ServiceName -> Click msg
serviceDeploysForService serviceName =
    toClick (Route.serviceDeploysForService serviceName)


serviceDeployForService : ServiceName -> ServiceHash -> Click msg
serviceDeployForService serviceName hash =
    toClick (Route.serviceDeployForService serviceName hash)


serviceDeploy : ServiceHash -> Click msg
serviceDeploy sh =
    toClick (Route.serviceDeploy sh)



-- VIEW


view : String -> Click msg -> Html msg
view label click =
    Click.view [] [ text label ] click


view_ : Html msg -> Click msg -> Html msg
view_ html click =
    Click.view [] [ html ] click



-- HELPERS


toClick : Route -> Click msg
toClick =
    Route.toUrlString >> Click.href
