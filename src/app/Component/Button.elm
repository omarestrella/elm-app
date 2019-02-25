module Component.Button exposing (primary)

import Html.Styled exposing (Html, button, text)
import Html.Styled.Attributes exposing (class)
import Html.Styled.Events exposing (onClick)


primary : String -> msg -> Html msg
primary t msg =
    button [ class "btn btn-primary", onClick msg ]
        [ text t ]
