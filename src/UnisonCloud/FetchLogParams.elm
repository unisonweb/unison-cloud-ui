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
