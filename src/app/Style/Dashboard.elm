module Style.Dashboard exposing (addNewBudgetGroup, budgetContainer, budgetGroupContainer, budgetGroupList, snapshotBalance, snapshotContainer, snapshotEntryContainer, snapshotEntryHeading, snapshotEntryValue, snapshotIncomeSpending)

import Css exposing (..)


budgetContainer =
    [ displayFlex
    , flexDirection column
    , padding (px 20)
    , width (pct 60)
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
    , boxShadow4 (px 0) (px 0) (px 6) (rgba 0 0 0 0.3)
    , padding (px 20)
    , borderRadius (px 8)
    , backgroundColor (hex "fff")
    ]


snapshotEntryContainer =
    [ displayFlex
    , flexDirection column
    , justifyContent center
    ]


snapshotEntryHeading =
    [ color (hex "#aaaaaa")
    , fontSize (em 0.9)
    , fontWeight bold
    ]


snapshotEntryValue =
    [ fontSize (em 1.3) ]


snapshotBalance =
    [ width (pct 30) ]


snapshotIncomeSpending =
    [ flex (int 1) ]
