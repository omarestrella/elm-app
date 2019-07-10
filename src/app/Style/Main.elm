module Style.Main exposing (navbar, navbarEntry)

import Css exposing (..)


navbar =
    [ displayFlex
    , padding4 (px 20) (px 20) (px 0) (px 20)
    ]


navbarEntry =
    [ padding2 (px 0) (px 8)
    , firstChild [ paddingLeft (px 0) ]
    ]
