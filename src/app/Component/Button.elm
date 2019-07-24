module Component.Button exposing (link, primary)

import Html.Styled exposing (Attribute, Html, button)
import Html.Styled.Attributes exposing (class)


primary : List (Attribute msg) -> List (Html msg) -> Html msg
primary attrs html =
    button (class "btn btn-primary" :: attrs)
        html


link : List (Attribute msg) -> List (Html msg) -> Html msg
link attrs html =
    button (class "btn btn-link" :: attrs)
        html
