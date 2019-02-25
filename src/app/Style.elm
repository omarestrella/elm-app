module Style exposing (accountsList, homeContainer)

import Css exposing (..)


homeContainer =
    [ displayFlex
    ]


accountsList =
    [ displayFlex
    , flexDirection column
    , width (px 200)
    ]
