module Money exposing (CurrencyCode(..), currencyCodeToSymbol, stringToCurrencyCode)


type CurrencyCode
    = USD
    | EUR
    | GBP
    | JPY
    | UnknownCurrency


stringToCurrencyCode : String -> CurrencyCode
stringToCurrencyCode str =
    case str of
        "USD" ->
            USD

        "EUR" ->
            EUR

        "GBP" ->
            GBP

        "JPY" ->
            JPY

        _ ->
            UnknownCurrency


currencyCodeToSymbol : CurrencyCode -> String
currencyCodeToSymbol code =
    case code of
        USD ->
            "$"

        EUR ->
            "€"

        GBP ->
            "£"

        JPY ->
            "¥"

        -- Something unique
        UnknownCurrency ->
            "₦"
