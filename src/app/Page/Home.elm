port module Page.Home exposing (Model, Msg, defaultModel, linkResponse, update, view)

import Html exposing (Html, button, div, text)
import Html.Attributes exposing (id)
import Html.Events exposing (onClick)
import Json.Decode as Decode
import Link exposing (LinkResponse(..))
import Model exposing (Account, ApiEnvironment(..), User)
import Session exposing (Session(..))


type Msg
    = NoOp
    | StartLink


defaultUser =
    { id = "0"
    , firstName = "First"
    , lastName = "Last"
    , email = "example@email.com"
    }


defaultModel session =
    { user = defaultUser
    , environment = Sandbox
    , linkResponse = LinkError []
    , accounts = []
    , session = session
    }


type alias Model =
    { user : User
    , environment : ApiEnvironment
    , linkResponse : LinkResponse
    , accounts : List Account
    , session : Session
    }



--Update


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        StartLink ->
            ( model, plaidLink "StartLink" )

        NoOp ->
            ( model, Cmd.none )



-- View


view : Model -> Html Msg
view model =
    div []
        [ text ("Welcome, " ++ model.user.firstName ++ " " ++ model.user.lastName)
        , button [ id "link-button", onClick StartLink ]
            [ text "Add Account" ]
        ]


port plaidLink : String -> Cmd msg


port linkResponse : (Decode.Value -> msg) -> Sub msg
