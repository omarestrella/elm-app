module Component.Input exposing (default)

import Html.Styled exposing (Attribute, Html, input)
import Html.Styled.Attributes exposing (class)


default : List (Attribute msg) -> List (Html msg) -> Html msg
default attrs html =
    input ([ class "form-control" ] ++ attrs)
        html
