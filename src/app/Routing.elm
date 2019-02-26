module Routing exposing (Route(..), extractRoute, parseRoute, routeLink, routePath, routeTo)

import Browser.Navigation as Navigation
import Html.Styled exposing (Html, a, text)
import Html.Styled.Attributes exposing (href)
import Html.Styled.Events exposing (onClick)
import Session exposing (Session, navKey)
import Url exposing (Url)
import Url.Parser as Parser exposing ((</>), Parser, int, map, oneOf, parse, s, top)


type Route
    = AuthRoute
    | DashboardRoute
    | AccountDetailRoute Int
    | NotFoundRoute


routeTo : Route -> Session -> Cmd msg
routeTo route session =
    Navigation.pushUrl (navKey session) (routePath route)


routeLink : String -> ( Route, Maybe msg ) -> Html msg
routeLink text_ ( route, handler ) =
    case handler of
        Nothing ->
            a [ href (routePath route) ]
                [ text text_ ]

        Just clickMsg ->
            a [ href (routePath route), onClick clickMsg ]
                [ text text_ ]


routePath : Route -> String
routePath route =
    case route of
        AuthRoute ->
            "auth"

        DashboardRoute ->
            "dashboard"

        _ ->
            "not-found"


parseRoute : Parser (Route -> a) a
parseRoute =
    oneOf
        [ map AuthRoute (s "auth")
        , map DashboardRoute
            (s "dashboard")
        , map AccountDetailRoute
            (s "accounts" </> int)
        ]


extractRoute : Url -> Route
extractRoute url =
    case parse parseRoute url of
        Just route ->
            route

        Nothing ->
            NotFoundRoute
