module UnisonCloud.Log exposing (..)

import Html exposing (Html, div)
import Html.Attributes exposing (class)
import Html.Events exposing (on)
import Http
import Json.Decode as Decode
import UI.DateTime exposing (DateTime)
import UI.Sizing as Sizing
import UnisonCloud.Env as Env exposing (Env)
import UnisonCloud.LogEntry exposing (LogEntry)



-- 2 directional infinite scroll
-- should start loading well before items are added
-- when items are added the scroll position shouldn't be affected... HOW?
-- https://addyosmani.com/blog/infinite-scroll-without-layout-shifts/
-- MODEL
{-

   Infinite Scroll Challenges
   --------------------------

   * There should not be any yank when scrolling up to look at older log messages
   * We should preload well ahead of the scroll bar, so there's no "loading"
   * We should not jump and yank the UI when new data is rendered
   * We need to scroll and load in both directions (maybe not in v1?)

   * https://addyosmani.com/blog/infinite-scroll-without-layout-shifts/

   One solution could be to pre-render a large empty area outside of the
   viewport, but this requires you to know the total number of entries and the
   height of an entry (the latter not being a big deal).

   Instagram doesn't seem to work that way. By looking at the scroll bar when
   scroll you can see that it jumps, but the page itself remains smooth, this
   indicates to me that there isn't a large empty area prerendered, but really
   good scroll position maintenance.

   We could render the new elements off to the side, in an iframe, measure the
   sizes of everything before adding them such that we can calculate the new
   scroll position perfectly and make the switch without the user knowing.

-}


type alias LogEntries =
    List LogEntry



-- WHERE IS BOOKMARK?


type Log
    = NotAsked
    | FirstTimeLoading
    | Loading LogEntries
    | Success LogEntries
    | Failure Http.Error


type Direction
    = Before
    | After


type alias Model =
    Log


init : Env -> ( Model, Cmd Msg )
init _ =
    ( NotAsked, Cmd.none )



-- UPDATE


type Msg
    = LogEntriesFetchFinished
    | Scroll
    | FetchMore


update : Env -> Msg -> Model -> ( Model, Cmd Msg )
update _ _ model =
    ( model, Cmd.none )



-- HELPERS


logEntryHeight : Sizing.Rem
logEntryHeight =
    Sizing.Rem 2



-- EFFECTS


fetchLogEntries : Env -> LogEntry -> Direction -> Cmd Msg
fetchLogEntries env bookmark direction =
    Cmd.none



-- VIEW


view : Model -> Html Msg
view _ =
    div [ on "scroll" (Decode.succeed Scroll), class "log" ] []
