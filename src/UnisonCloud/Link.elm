module UnisonCloud.Link exposing (..)

import Html exposing (Html, text)
import UI.Click as Click exposing (Click)
import UnisonCloud.Route as Route exposing (Route)
import UnisonCloud.ServiceHash exposing (ServiceHash)



{-

   Link
   ====

   Various UI.Click link helpers for Routes and external links

-}
-- EXTERNAL URLS


link : String -> Click msg
link url =
    Click.externalHref url


unisonCloudWebsite : Click msg
unisonCloudWebsite =
    Click.externalHref "https://unison.cloud"


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


tour : Click msg
tour =
    Click.externalHref "https://unison-lang.org/docs/tour"


codeOfConduct : Click msg
codeOfConduct =
    Click.externalHref "https://www.unison-lang.org/community/code-of-conduct/"


status : Click msg
status =
    Click.externalHref "https://unison.statuspage.io"


slack : Click msg
slack =
    Click.externalHref "https://unison-lang.com/slack"


login : Click msg
login =
    -- TODO: Use Env.apiUrl
    Click.externalHref "https://api.unison.cloud/login"


logout : Click msg
logout =
    -- TODO: Use Env.apiUrl
    Click.externalHref "https://api.unison.cloud/logout"



-- INTERNAL URLS


overview : Click msg
overview =
    toClick Route.overview


service : ServiceHash -> Click msg
service sh =
    toClick (Route.service sh)



-- VIEW


view : String -> Click msg -> Html msg
view label click =
    Click.view [] [ text label ] click



-- HELPERS


toClick : Route -> Click msg
toClick =
    Route.toUrlString >> Click.href
