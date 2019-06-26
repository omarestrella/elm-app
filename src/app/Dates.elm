module Dates exposing (DateRange(..), dateRangeToLabel, dateRangeToQuery, dateRangeToString, dateSelector, formattedTransactionGroupDate, stringToDateRange)

import DateFormat
import Derberos.Date.Calendar as Calendar
import Derberos.Date.Core as Core
import Derberos.Date.Delta as Delta
import Derberos.Date.Utils as Utils
import Html.Styled exposing (Html, option, select, text)
import Html.Styled.Attributes exposing (value)
import Html.Styled.Events exposing (onInput)
import Iso8601
import Time exposing (Month(..), Posix, Zone)


type DateRange
    = ThisMonth
    | LastMonth
    | Last30Days
    | Last90Days


type alias DateRangeQuery =
    { start : Int
    , end : Int
    }



-- Internal


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


stringToPosix : String -> Posix
stringToPosix string =
    let
        possibleDate =
            Iso8601.toTime string
    in
    case possibleDate of
        Ok posix ->
            let
                year =
                    posix |> Time.toYear Time.utc

                month =
                    posix |> Time.toMonth Time.utc |> monthToInt

                day =
                    posix |> Time.toDay Time.utc

                record =
                    Core.newDateRecord year month day 0 0 0 0 Time.utc
            in
            Core.civilToPosix record

        Err _ ->
            Time.millisToPosix 0



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


dateRangeToQuery : DateRange -> Posix -> DateRangeQuery
dateRangeToQuery range baseTime =
    let
        zone =
            Time.utc

        toYear =
            Time.toYear zone

        toMonth =
            Time.toMonth zone >> monthToInt

        toDay =
            Time.toDay zone
    in
    case range of
        ThisMonth ->
            { start = Calendar.getFirstDayOfMonth zone baseTime |> Time.posixToMillis
            , end = Calendar.getLastDayOfMonth zone baseTime |> Time.posixToMillis
            }

        LastMonth ->
            let
                date =
                    Delta.addMonths -1 zone baseTime
            in
            { start = Calendar.getFirstDayOfMonth zone date |> Time.posixToMillis
            , end = Calendar.getLastDayOfMonth zone date |> Time.posixToMillis
            }

        Last30Days ->
            let
                date =
                    Delta.addDays -30 baseTime
            in
            { start = Calendar.getFirstDayOfMonth zone date |> Time.posixToMillis
            , end = baseTime |> Time.posixToMillis
            }

        Last90Days ->
            let
                date =
                    Delta.addDays -90 baseTime
            in
            { start = Calendar.getFirstDayOfMonth zone date |> Time.posixToMillis
            , end = baseTime |> Time.posixToMillis
            }


formattedTransactionGroupDate : String -> String
formattedTransactionGroupDate str =
    let
        date =
            stringToPosix str
    in
    DateFormat.format
        [ DateFormat.monthNameAbbreviated
        , DateFormat.text " "
        , DateFormat.dayOfMonthFixed
        , DateFormat.text ", "
        , DateFormat.yearNumber
        ]
        Time.utc
        date



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
