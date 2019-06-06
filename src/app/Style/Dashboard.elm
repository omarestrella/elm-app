module Style.Dashboard exposing (addNewBudgetGroup, budgetContainer, budgetGroupContainer, budgetGroupList, columnDisplay, snapshotBalance, snapshotContainer, snapshotEntry, snapshotEntryContainer, snapshotEntryHeading, snapshotEntryValue, snapshotIncomeSpending, transactionDetail, transactionDetailAmount, transactionDetailAmountColor, transactionDetailCategory, transactionDetailName, transactionDetailNameCategory, transactionGroup, transactionGroupDetail, transactionGroupTitle)

import Css exposing (..)
import Css.Media exposing (maxWidth, only, screen, withMedia)


columnDisplay =
    [ displayFlex
    , flexDirection column
    ]


budgetContainer =
    [ displayFlex
    , flexDirection column
    , padding (px 20)
    , width (pct 60)
    , withMedia [ only screen [ maxWidth (px 600) ] ]
        [ width (pct 100) ]
    ]


budgetGroupList =
    [ displayFlex
    , flexDirection column
    ]


budgetGroupContainer =
    [ boxShadow4 (px 0) (px 0) (px 3) (rgba 0 0 0 0.3)
    , padding (px 10)
    ]


addNewBudgetGroup =
    budgetGroupContainer
        ++ [ marginTop (px 10)
           ]


snapshotContainer =
    [ displayFlex
    , boxShadow4 (px 0) (px 0) (px 8) (rgba 0 0 0 0.2)
    , padding (px 20)
    , borderRadius (px 8)
    , backgroundColor (hex "fff")
    ]


snapshotEntryContainer =
    [ displayFlex
    , flexDirection column
    , justifyContent center
    ]


snapshotEntry =
    [ marginBottom (rem 0.5) ]


snapshotEntryHeading =
    [ color (hex "#aaaaaa")
    , fontSize (em 0.9)
    , fontWeight bold
    , marginBottom (rem 0.3)
    ]


snapshotEntryValue =
    [ fontSize (em 1.3) ]


snapshotBalance =
    [ width (pct 30) ]


snapshotIncomeSpending =
    [ flex (int 1) ]


transactionGroup =
    [ displayFlex
    , flexDirection column
    , marginTop (px 10)
    , marginBottom (px 10)
    ]


transactionGroupTitle =
    [ color (hex "#aaa")
    , fontSize (em 0.8)
    , margin (px 0)
    , paddingBottom (px 4)
    ]


transactionGroupDetail =
    [ backgroundColor (hex "#fff")
    , borderRadius (px 4)
    , boxShadow4 (px 0) (px 0) (px 8) (rgba 0 0 0 0.2)
    , padding (px 12)
    ]


transactionDetail =
    [ displayFlex
    , alignItems center
    , paddingBottom (px 4)
    , marginBottom (px 4)
    , borderBottom3 (px 1) solid (hex "#ddd")
    , lastChild
        [ border (px 0)
        , marginBottom (px 0)
        , paddingBottom (px 0)
        ]
    ]


transactionDetailNameCategory =
    columnDisplay
        ++ [ flex (int 1)
           ]


transactionDetailName =
    [ flex (int 1)
    , fontSize (em 0.9)
    ]


transactionDetailCategory =
    [ fontSize (em 0.85)
    , color (hex "#aaa")
    ]


transactionDetailAmount =
    [ fontSize (em 0.9) ]


transactionDetailAmountColor transaction =
    if transaction.amount > 0 then
        [ color (hex "#f73650") ]

    else
        [ color (hex "#6ee853") ]
