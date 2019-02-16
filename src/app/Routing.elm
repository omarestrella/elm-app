module Routing exposing (Route(..), extractRoute, parseRoute, routePath, routeTo)

import Browser.Navigation as Navigation
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
    Navigation.pushUrl (navKey session) (routePath DashboardRoute)


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
