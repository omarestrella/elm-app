port module Main exposing (Model, Msg(..), init, main, subscriptions, update, updateWith, view)

import Browser exposing (Document, UrlRequest(..))
import Browser.Navigation as Navigation
import Html exposing (Html, a, button, div, form, h1, img, input, label, text)
import Html.Attributes exposing (href, id, src, type_)
import Html.Events exposing (onClick, onSubmit)
import Http
import Json.Decode as Decode
import Json.Encode as Encode
import Link exposing (Link, LinkResponse(..), linkResponseDecoder)
import LocalStorage as Storage
import Page.Auth as Auth
import Page.Home as Home
import Routing exposing (..)
import Session exposing (Session(..), SessionData, getSession)
import Url exposing (Url)



---- MODEL ----


type Model
    = Loading Navigation.Key
    | Ready AppModel


type alias AppModel =
    { currentRoute : Route
    , pageModel : PageModel
    , session : Session
    }


type PageModel
    = Auth Auth.Model
    | Home Home.Model


type Msg
    = NoOp
    | ChangedUrl Url
    | ClickedLink UrlRequest
    | GotSession String (Result Http.Error Session.User)
    | GotAuthMsg Auth.Msg
    | GotHomeMsg Home.Msg


init : Maybe String -> Url -> Navigation.Key -> ( Model, Cmd Msg )
init accessToken url key =
    let
        guestSession =
            Guest key
    in
    case accessToken of
        Nothing ->
            ( Ready
                { currentRoute = AuthRoute
                , pageModel = Auth (Auth.defaultModel guestSession)
                , session = guestSession
                }
            , Navigation.pushUrl key (routePath AuthRoute)
            )

        Just token ->
            ( Loading key
            , getSession (GotSession token) token
            )


defaultPageModel : Session -> Route -> PageModel
defaultPageModel session route =
    let
        navKey =
            Session.navKey session
    in
    case route of
        AuthRoute ->
            Auth (Auth.defaultModel session)

        DashboardRoute ->
            Home (Home.defaultModel session)

        _ ->
            Auth (Auth.defaultModel session)


pageModelSession : PageModel -> Session
pageModelSession pageModel =
    case pageModel of
        Auth model ->
            model.session

        Home model ->
            model.session



---- UPDATE ----


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case model of
        Loading key ->
            case msg of
                GotSession token result ->
                    let
                        guestSession =
                            Guest key
                    in
                    case result of
                        Ok user ->
                            let
                                authSession =
                                    LoggedIn key
                                        { accessToken = token
                                        , user = user
                                        , accounts = []
                                        }

                                appModel =
                                    { currentRoute = DashboardRoute
                                    , pageModel = Home (Home.defaultModel authSession)
                                    , session = authSession
                                    }
                            in
                            ( Ready appModel
                            , Cmd.batch
                                [ Routing.routeTo DashboardRoute authSession
                                , Storage.set "accessToken" token
                                ]
                            )

                        Err err ->
                            let
                                _ =
                                    Debug.log "Err" err

                                appModel =
                                    { currentRoute = AuthRoute
                                    , pageModel = Auth (Auth.defaultModel guestSession)
                                    , session = guestSession
                                    }
                            in
                            ( Ready appModel, Navigation.pushUrl key (routePath AuthRoute) )

                _ ->
                    ( model, Cmd.none )

        Ready appModel ->
            let
                navKey =
                    Session.navKey appModel.session
            in
            readyUpdate msg appModel navKey


readyUpdate : Msg -> AppModel -> Navigation.Key -> ( Model, Cmd Msg )
readyUpdate msg model navKey =
    case ( msg, model.pageModel ) of
        ( GotAuthMsg authMsg, Auth authModel ) ->
            let
                _ =
                    Debug.log "GotAuthMsg Model:" authModel

                _ =
                    Debug.log "GotAuthMsg Msg:" authMsg
            in
            Auth.update authMsg authModel
                |> updateWith Auth GotAuthMsg model

        ( GotHomeMsg homeMsg, Home homeModel ) ->
            Home.update homeMsg homeModel
                |> updateWith Home GotHomeMsg model

        ( ChangedUrl url, _ ) ->
            let
                route =
                    Routing.extractRoute url
            in
            ( Ready
                { model
                    | currentRoute = route
                    , pageModel = defaultPageModel model.session route
                }
            , Cmd.none
            )

        ( ClickedLink request, _ ) ->
            case request of
                Internal url ->
                    let
                        route =
                            extractRoute url
                    in
                    ( Ready
                        { model
                            | currentRoute = route
                            , pageModel = defaultPageModel model.session route
                        }
                    , Navigation.pushUrl navKey (routePath route)
                    )

                External url ->
                    ( Ready model, Navigation.load url )

        -- The following do nothing
        ( NoOp, _ ) ->
            ( Ready model, Cmd.none )

        ( GotSession _ _, _ ) ->
            ( Ready model, Cmd.none )

        ( GotAuthMsg _, Home _ ) ->
            ( Ready model, Cmd.none )

        ( GotHomeMsg _, Auth _ ) ->
            ( Ready model, Cmd.none )


updateWith : (subModel -> PageModel) -> (subMsg -> Msg) -> AppModel -> ( subModel, Cmd subMsg ) -> ( Model, Cmd Msg )
updateWith toModel toMsg model ( subModel, subCmd ) =
    let
        pageModel =
            toModel subModel
    in
    ( Ready { model | pageModel = pageModel, session = pageModelSession pageModel }
    , Cmd.map toMsg subCmd
    )



---- VIEW ----


pageView : AppModel -> Html Msg
pageView model =
    case model.pageModel of
        Auth authModel ->
            Auth.view authModel |> Html.map GotAuthMsg

        Home homeModel ->
            Home.view homeModel |> Html.map GotHomeMsg


navbarView : Route -> Html Msg
navbarView route =
    div []
        [ a [ href (routePath DashboardRoute) ]
            [ text "Dashboard" ]
        , a [ href (routePath AuthRoute) ]
            [ text "Login/Register" ]
        ]


bodyView : Model -> Html Msg
bodyView model =
    case model of
        Loading _ ->
            div [] [ text "Loading..." ]

        Ready appModel ->
            div []
                [ navbarView appModel.currentRoute
                , pageView appModel
                ]


view : Model -> Browser.Document Msg
view model =
    { title = "Budget"
    , body = [ bodyView model ]
    }



---- PROGRAM ----


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.batch
        [ Home.subscriptions |> Sub.map GotHomeMsg
        ]


main : Program (Maybe String) Model Msg
main =
    Browser.application
        { init = init
        , view = view
        , update = update
        , subscriptions = subscriptions
        , onUrlRequest = ClickedLink
        , onUrlChange = ChangedUrl
        }
