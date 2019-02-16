port module Page.Home exposing (Model, Msg, defaultModel, linkResponse, subscriptions, update, view)

import Html exposing (Html, button, div, text)
import Html.Attributes exposing (id)
import Html.Events exposing (onClick)
import Http
import Json.Decode as Decode
import Link exposing (LinkResponse(..), linkResponseDecoder)
import Model exposing (Account, ApiEnvironment(..), User)
import Session exposing (Session(..))


type Msg
    = NoOp
    | StartLink
    | LinkResponseMsg (Result Decode.Error LinkResponse)
    | HandleItemLink (Result Http.Error Session.Item)


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

        LinkResponseMsg response ->
            let
                _ =
                    Debug.log "Response" response

                token =
                    Session.accessToken model.session
            in
            case response of
                Ok linkData ->
                    case linkData of
                        LinkSuccess link ->
                            ( model
                            , Session.linkItemToUser HandleItemLink token link.publicToken
                            )

                        LinkError errors ->
                            -- TODO: graceful error handling
                            ( model, Cmd.none )

                Err _ ->
                    -- TODO: graceful error handling
                    ( model, Cmd.none )

        HandleItemLink response ->
            case response of
                Ok item ->
                    ( { model
                        | session = Session.addItemToUser model.session item
                      }
                    , Cmd.none
                    )

                Err _ ->
                    ( model, Cmd.none )

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


subscriptions : Sub Msg
subscriptions =
    linkResponse
        (\value ->
            LinkResponseMsg (Decode.decodeValue linkResponseDecoder value)
        )
