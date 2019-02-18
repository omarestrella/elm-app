module Page.Auth exposing (LoginForm, Model, Msg(..), RegistrationForm, defaultModel, login, loginFormView, register, registerFormView, update, updateLoginForm, updateRegistrationForm, view)

import Api exposing (Method(..))
import Html exposing (Html, button, div, input, label, text)
import Html.Attributes exposing (type_)
import Html.Events exposing (onInput, onSubmit)
import Http
import Json.Decode as Decode exposing (field, int, string)
import Json.Encode as Encode
import Link exposing (Link)
import LocalStorage as Storage
import Routing exposing (Route(..), routePath)
import Session exposing (Session(..), SessionData, decoder, navKey)



-- Model


type Msg
    = Login String String
    | Register RegistrationForm
    | GotLogin (Result Http.Error LoginResponse)
    | GotRegister (Result Http.Error String)
    | EnteredLoginEmail String
    | EnteredLoginPassword String
    | EnteredRegistrationEmail String
    | EnteredRegistrationPassword String
    | EnteredRegistrationFirstName String
    | EnteredRegistrationLastName String


type alias Model =
    { loginForm : LoginForm
    , registrationForm : RegistrationForm
    , session : Session
    }


type alias LoginForm =
    { email : String
    , password : String
    }


type alias RegistrationForm =
    { email : String
    , password : String
    , firstName : String
    , lastName : String
    }


type alias LoginResponse =
    { user : Session.User
    , accessToken : String
    }


defaultModel : Session -> Model
defaultModel session =
    { loginForm = { email = "", password = "" }
    , registrationForm = { email = "", password = "", firstName = "", lastName = "" }
    , session = session
    }



-- Update


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        Login email password ->
            ( model, login email password )

        Register form ->
            ( model, register form )

        GotLogin response ->
            case response of
                Ok loginResponse ->
                    let
                        key =
                            navKey model.session

                        newSession =
                            LoggedIn key
                                { accessToken = loginResponse.accessToken
                                , user = loginResponse.user
                                , accounts = []
                                }

                        newModel =
                            { model | session = newSession }
                    in
                    ( newModel
                    , Cmd.batch
                        [ Routing.routeTo DashboardRoute newModel.session
                        , Storage.set "accessToken" loginResponse.accessToken
                        ]
                    )

                Err err ->
                    let
                        _ =
                            Debug.log "Login error" err
                    in
                    ( model, Cmd.none )

        GotRegister response ->
            let
                _ =
                    Debug.log "got register" response
            in
            ( model, Cmd.none )

        EnteredLoginEmail value ->
            updateLoginForm (\form -> { form | email = value }) model

        EnteredLoginPassword value ->
            updateLoginForm (\form -> { form | password = value }) model

        EnteredRegistrationEmail value ->
            updateRegistrationForm (\form -> { form | email = value }) model

        EnteredRegistrationPassword value ->
            updateRegistrationForm (\form -> { form | password = value }) model

        EnteredRegistrationFirstName value ->
            updateRegistrationForm (\form -> { form | firstName = value }) model

        EnteredRegistrationLastName value ->
            updateRegistrationForm (\form -> { form | lastName = value }) model


updateRegistrationForm : (RegistrationForm -> RegistrationForm) -> Model -> ( Model, Cmd Msg )
updateRegistrationForm transformer model =
    ( { model | registrationForm = transformer model.registrationForm }, Cmd.none )


updateLoginForm : (LoginForm -> LoginForm) -> Model -> ( Model, Cmd Msg )
updateLoginForm transformer model =
    ( { model | loginForm = transformer model.loginForm }, Cmd.none )


login : String -> String -> Cmd Msg
login email password =
    let
        body =
            Encode.object
                [ ( "email", Encode.string email )
                , ( "password", Encode.string password )
                ]
    in
    Api.request
        { url = "/auth/login"
        , accessToken = Nothing
        , method = POST
        , body = Just (Http.jsonBody body)
        , handler = Http.expectJson GotLogin loginDecoder
        }


register : RegistrationForm -> Cmd Msg
register form =
    let
        body =
            Encode.object
                [ ( "email", Encode.string form.email )
                , ( "password", Encode.string form.password )
                , ( "givenName", Encode.string form.firstName )
                , ( "familyName", Encode.string form.lastName )
                ]
    in
    Http.post
        { url = "/api/v1/auth/register"
        , body = Http.jsonBody body
        , expect = Http.expectString GotRegister
        }



-- View


loginFormView : LoginForm -> Html Msg
loginFormView loginForm =
    Html.form [ onSubmit (Login loginForm.email loginForm.password) ]
        [ div []
            [ label [] [ text "email" ]
            , input
                [ onInput EnteredLoginEmail ]
                []
            ]
        , div []
            [ label [] [ text "password" ]
            , input
                [ type_ "password", onInput EnteredLoginPassword ]
                []
            ]
        , button []
            [ text "Login" ]
        ]


registerFormView : RegistrationForm -> Html Msg
registerFormView form =
    Html.form [ onSubmit (Register form) ]
        [ div []
            [ label [] [ text "email" ]
            , input [ onInput EnteredRegistrationEmail ] []
            ]
        , div []
            [ label [] [ text "password" ]
            , input
                [ type_ "password"
                , onInput EnteredRegistrationPassword
                ]
                []
            ]
        , div []
            [ label [] [ text "first name" ]
            , input
                [ onInput EnteredRegistrationFirstName ]
                []
            ]
        , div []
            [ label [] [ text "last name" ]
            , input
                [ onInput EnteredRegistrationLastName ]
                []
            ]
        , button []
            [ text "Login" ]
        ]


view : Model -> Html Msg
view model =
    div []
        [ loginFormView model.loginForm
        , registerFormView model.registrationForm
        ]



-- Decoders


loginDecoder : Decode.Decoder LoginResponse
loginDecoder =
    Decode.map2 LoginResponse
        (field "user" Session.userDecoder)
        (field "accessToken" string)
