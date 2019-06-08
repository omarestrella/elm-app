module Dates exposing (DateRange(..), dateRangeToLabel, dateRangeToQuery, dateRangeToString, dateSelector, stringToDateRange)

import Derberos.Date.Calendar as Calendar
import Derberos.Date.Delta as Delta
import Html.Styled exposing (Html, option, select, text)
import Html.Styled.Attributes exposing (value)
import Html.Styled.Events exposing (onInput)
import Time exposing (Month(..), Posix, Zone)


type DateRange
    = ThisMonth
    | LastMonth
    | Last30Days
    | Last90Days


type alias DateRangeQuery =
    { start : String
    , end : String
    }



-- Internal


queryDateString : Int -> Int -> Int -> String
queryDateString year month day =
    String.fromInt year
        ++ "-"
        ++ String.fromInt month
        ++ "-"
        ++ String.fromInt day


monthToInt : Time.Month -> Int
monthToInt month =
    case month of
        Jan ->
            1

        Feb ->
            2

        Mar ->
            3

        Apr ->
            4

        May ->
            5

        Jun ->
            6

        Jul ->
            7

        Aug ->
            8

        Sep ->
            9

        Oct ->
            10

        Nov ->
            11

        Dec ->
            12



-- Helpers


dateRangeToString : DateRange -> String
dateRangeToString range =
    case range of
        ThisMonth ->
            "ThisMonth"

        LastMonth ->
            "LastMonth"

        Last30Days ->
            "Last30Days"

        Last90Days ->
            "Last90Days"


stringToDateRange : String -> DateRange
stringToDateRange str =
    case str of
        "ThisMonth" ->
            ThisMonth

        "LastMonth" ->
            LastMonth

        "Last30Days" ->
            Last30Days

        "Last90Days" ->
            Last90Days

        _ ->
            ThisMonth


dateRangeToLabel : DateRange -> String
dateRangeToLabel range =
    case range of
        ThisMonth ->
            "This month"

        LastMonth ->
            "Last month"

        Last30Days ->
            "Last 30 days"

        Last90Days ->
            "Last 90 days"


dateRangeToQuery : DateRange -> Zone -> Posix -> DateRangeQuery
dateRangeToQuery range zone baseTime =
    let
        toYear =
            Time.toYear zone

        toMonth =
            Time.toMonth zone >> monthToInt

        toDay =
            Time.toDay zone

        format =
            \posix ->
                queryDateString (toYear posix) (toMonth posix) (toDay posix)
    in
    case range of
        ThisMonth ->
            { start = Calendar.getFirstDayOfMonth zone baseTime |> format
            , end = Calendar.getLastDayOfMonth zone baseTime |> format
            }

        LastMonth ->
            let
                date =
                    Delta.addMonths -1 zone baseTime
            in
            { start = Calendar.getFirstDayOfMonth zone date |> format
            , end = Calendar.getLastDayOfMonth zone date |> format
            }

        Last30Days ->
            let
                date =
                    Delta.addDays -30 baseTime
            in
            { start = Calendar.getFirstDayOfMonth zone date |> format
            , end = baseTime |> format
            }

        Last90Days ->
            let
                date =
                    Delta.addDays -90 baseTime
            in
            { start = Calendar.getFirstDayOfMonth zone date |> format
            , end = baseTime |> format
            }



-- Views


dateSelector : List DateRange -> (String -> msg) -> Html msg
dateSelector ranges action =
    select [ onInput action ]
        (List.map
            (\range ->
                option
                    [ value (dateRangeToString range) ]
                    [ text (dateRangeToLabel range) ]
            )
            ranges
        )
