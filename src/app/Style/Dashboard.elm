module Style.Dashboard exposing (addNewBudgetGroup, budgetContainer, budgetGroupContainer, budgetGroupList)

import Css exposing (..)


budgetContainer =
    [ displayFlex
    , flexDirection column
    , padding (px 20)
    , width (pct 70)
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
