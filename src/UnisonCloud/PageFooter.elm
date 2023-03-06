module UnisonCloud.PageFooter exposing (..)

import UI.PageLayout exposing (PageFooter(..))
import UnisonCloud.Link as Link


pageFooter : PageFooter msg
pageFooter =
    PageFooter [ Link.view "Status" Link.status ]
