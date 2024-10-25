module UnisonCloud.FetchLogParams exposing
    ( Direction(..)
    , FetchLogParams
    , fetchLogParams
    , toQueryParams
    , withDirection
    , withEnd
    , withLimit
    , withSearch
    , withStart
    )

import Maybe.Extra as MaybeE
import UI.DateTime as DateTime exposing (DateTime)
import Url.Builder exposing (QueryParameter, int, string)


type Direction
    = Backward
    | Forward


type alias FetchLogParams =
    { search : Maybe String
    , start : Maybe DateTime
    , end : Maybe DateTime
    , direction : Direction
    , limit : Int
    }



-- CREATE


fetchLogParams : FetchLogParams
fetchLogParams =
    { search = Nothing
    , start = Nothing
    , end = Nothing
    , direction = Backward
    , limit = 100
    }



-- MODIFY


withSearch : String -> FetchLogParams -> FetchLogParams
withSearch search params =
    { params | search = Just search }


withStart : DateTime -> FetchLogParams -> FetchLogParams
withStart start params =
    { params | start = Just start }


withEnd : DateTime -> FetchLogParams -> FetchLogParams
withEnd end params =
    { params | end = Just end }


withDirection : Direction -> FetchLogParams -> FetchLogParams
withDirection d params =
    { params | direction = d }


withLimit : Int -> FetchLogParams -> FetchLogParams
withLimit l params =
    { params | limit = l }



-- TRANSFORM
{-
   From https://grafana.com/docs/loki/latest/reference/loki-http-api/#query-logs-within-a-range-of-time:
      `start`:
        The start time for the query as a nanosecond Unix epoch or another
        supported format. Defaults to one hour ago. Loki returns results with timestamp
        greater or equal to this value.

        If `start is not provided, we will default it to the service deploy time.
      `end`:
        The end time for the query as a nanosecond Unix epoch or another supported
        format. Defaults to now. Loki returns results with timestamp lower than this
        value.

        If `end is not provided, loki will default this to "now".
      `direction`:
        Determines the sort order of logs. Supported values are forward or backward.
        Defaults to backward.
      `limit`:
        The max number of entries to return. It defaults to 100. Only applies
        to query types which produce a stream (log lines) response.
-}


toQueryParams : DateTime -> DateTime -> FetchLogParams -> List QueryParameter
toQueryParams fourtyEightHoursAgo now p =
    let
        search =
            Maybe.map (string "search") p.search

        start =
            p.start
                |> MaybeE.orElse (Just now)
                |> Maybe.map DateTime.toISO8601
                |> Maybe.map (string "start")

        end =
            p.end
                |> MaybeE.orElse (Just fourtyEightHoursAgo)
                |> Maybe.map DateTime.toISO8601
                |> Maybe.map (string "end")

        direction =
            Just (string "direction" (directionToString p.direction))

        limit =
            Just (int "limit" p.limit)
    in
    MaybeE.values [ search, start, end, direction, limit ]


directionToString : Direction -> String
directionToString dir =
    case dir of
        Backward ->
            "backward"

        Forward ->
            "forward"
