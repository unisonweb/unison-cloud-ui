module UnisonCloud.LogEntries exposing (..)

import Time
import UI.DateTime as DateTime exposing (DateTime)
import UnisonCloud.LogLine exposing (LogLine)


type LogEntry
    = Line LogLine
    | DateBoundary DateTime


type alias LogEntries =
    List LogEntry


{-| LogLines are ordered (from the backend) with the newest last in the list, which is how we want to
render them, however, we use css to reverse the rendering of the list (which
gives us some better scrolling behavior and control), so while we're adding
date markers, by running through the list, we're also flipping its order, so
that when css again flips it, it will be rendered with the newest entry in the
bottom of the screen, like you'd see with `tail`.
-}
fromLines : Time.Zone -> List LogLine -> List LogEntry
fromLines timeZone lines =
    let
        f l ( entries, currentDate ) =
            case currentDate of
                Nothing ->
                    ( [ Line l ], Just l.loggedAt )

                Just d ->
                    if DateTime.isSameDay timeZone l.loggedAt d then
                        ( entries ++ [ Line l ], Just l.loggedAt )

                    else
                        ( entries ++ [ DateBoundary l.loggedAt, Line l ]
                        , Just l.loggedAt
                        )
    in
    lines
        |> List.foldl f ( [], Nothing )
        |> Tuple.first
        |> List.reverse
