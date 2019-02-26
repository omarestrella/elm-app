port module Main exposing (Model, Msg(..), init, main, subscriptions, update, updateWith, view)

import Browser exposing (Document, UrlRequest(..))
import Browser.Navigation as Navigation
import Html.Styled exposing (Html, a, button, div, form, h1, img, input, label, map, text, toUnstyled)
import Html.Styled.Attributes exposing (href, id, src, type_)
import Html.Styled.Events exposing (onClick, onSubmit)
import Http
import Json.Decode as Decode
import Json.Encode as Encode
import Link exposing (Link, LinkResponse(..), linkResponseDecoder)
import LocalStorage as Storage
import Page.Auth as Auth
import Page.Dashboard as Dashboard
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
    | Dashboard Dashboard.Model


type Msg
    = NoOp
    | ChangedUrl Url
    | ClickedLink UrlRequest
    | GotSession String (Result Http.Error Session.User)
    | GotAccounts (Result Http.Error (List Session.Account))
    | GotAuthMsg Auth.Msg
    | GotDashboardMsg Dashboard.Msg


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
            Dashboard (Dashboard.defaultModel session)

        _ ->
            Auth (Auth.defaultModel session)


pageModelSession : PageModel -> Session
pageModelSession pageModel =
    case pageModel of
        Auth model ->
            model.session

        Dashboard model ->
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
                                    , pageModel = Dashboard (Dashboard.defaultModel authSession)
                                    , session = authSession
                                    }

                                accountTokens =
                                    List.map .accessToken user.items
                            in
                            ( Ready appModel
                            , Cmd.batch
                                [ Routing.routeTo DashboardRoute authSession
                                , Storage.set "accessToken" token
                                , Session.loadAccounts GotAccounts token accountTokens
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
                            ( Ready appModel, Routing.routeTo AuthRoute guestSession )

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

        ( GotDashboardMsg homeMsg, Dashboard homeModel ) ->
            Dashboard.update homeMsg homeModel
                |> updateWith Dashboard GotDashboardMsg model

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

        ( GotAccounts response, Dashboard homeModel ) ->
            case response of
                Ok accounts ->
                    let
                        pageModel =
                            { homeModel | session = Session.addAccountsToSession homeModel.session accounts }
                    in
                    ( Ready { model | pageModel = Dashboard pageModel }, Cmd.none )

                Err _ ->
                    ( Ready model, Cmd.none )

        -- The following do nothing
        ( NoOp, _ ) ->
            ( Ready model, Cmd.none )

        ( GotSession _ _, _ ) ->
            ( Ready model, Cmd.none )

        ( GotAccounts response, _ ) ->
            ( Ready model, Cmd.none )

        ( GotAuthMsg _, Dashboard _ ) ->
            ( Ready model, Cmd.none )

        ( GotDashboardMsg _, Auth _ ) ->
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


routeHandler : Route -> ( Route, Maybe Msg )
routeHandler route =
    case route of
        DashboardRoute ->
            ( DashboardRoute, Just (GotDashboardMsg Dashboard.routeHandler) )

        AuthRoute ->
            ( AuthRoute, Nothing )

        AccountDetailRoute id ->
            ( AccountDetailRoute id, Nothing )

        NotFoundRoute ->
            ( NotFoundRoute, Nothing )



---- VIEW ----


pageView : AppModel -> Html Msg
pageView model =
    case model.pageModel of
        Auth authModel ->
            Auth.view authModel
                |> map GotAuthMsg

        Dashboard dashboardModel ->
            Dashboard.view dashboardModel
                |> map GotDashboardMsg


navbarView : Route -> Html Msg
navbarView route =
    div []
        [ Routing.routeLink "Dashboard" (routeHandler DashboardRoute)
        , Routing.routeLink "Login/Register" (routeHandler AuthRoute)
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
    , body = [ toUnstyled <| bodyView model ]
    }



---- PROGRAM ----


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.batch
        [ Dashboard.subscriptions |> Sub.map GotDashboardMsg
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
