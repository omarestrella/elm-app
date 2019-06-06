module Dates exposing (DateRange(..), dateRangeToLabel, dateRangeToString, dateSelector, stringToDateRange)

import Html.Styled exposing (Html, option, select, text)
import Html.Styled.Attributes exposing (value)
import Html.Styled.Events exposing (onInput)


type DateRange
    = ThisMonth
    | LastMonth
    | Last30Days
    | Last90Days



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
