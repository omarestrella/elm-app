module Component.Button exposing (link, primary)

import Html.Styled exposing (Attribute, Html, button, text)
import Html.Styled.Attributes exposing (class)
import Html.Styled.Events exposing (onClick)


primary : List (Attribute msg) -> List (Html msg) -> Html msg
primary attrs html =
    button ([ class "btn btn-primary" ] ++ attrs)
        html


link : List (Attribute msg) -> List (Html msg) -> Html msg
link attrs html =
    button ([ class "btn btn-link" ] ++ attrs)
        html
