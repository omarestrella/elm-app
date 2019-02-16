module Session exposing (Item, Session(..), SessionData, User, accessToken, addItemToUser, decoder, encode, getSession, linkItemToUser, navKey, userDecoder)

import Api exposing (Method(..))
import Browser.Navigation as Navigation
import Http
import Json.Decode as Decode exposing (field, int, list, string)
import Json.Encode as Encode


type Session
    = Guest Navigation.Key
    | LoggedIn Navigation.Key String User


type alias User =
    { id : Int
    , firstName : String
    , lastName : String
    , email : String
    , items : List Item
    }


type alias Item =
    { id : Int
    , itemId : String
    , accessToken : String
    }


type alias SessionData =
    { user : User
    , accessToken : String
    }



-- Utils


addItemToUser : Session -> Item -> Session
addItemToUser session item =
    case session of
        Guest _ ->
            session

        LoggedIn key token user ->
            LoggedIn key token { user | items = user.items ++ [ item ] }


navKey : Session -> Navigation.Key
navKey session =
    case session of
        Guest key ->
            key

        LoggedIn key _ _ ->
            key


accessToken : Session -> String
accessToken session =
    case session of
        Guest _ ->
            ""

        LoggedIn _ token _ ->
            token


getSession : (Result Http.Error User -> msg) -> String -> Cmd msg
getSession msg token =
    Api.request
        { url = "/auth/session"
        , accessToken = Just token
        , method = POST
        , body = Nothing
        , handler = Http.expectJson msg decoder
        }


linkItemToUser : (Result Http.Error Item -> msg) -> String -> String -> Cmd msg
linkItemToUser msg token publicToken =
    let
        body =
            Encode.object
                [ ( "publicToken", Encode.string publicToken )
                ]
    in
    Api.request
        { url = "/items"
        , accessToken = Just token
        , method = POST
        , body = Just (Http.jsonBody body)
        , handler = Http.expectJson msg itemDecoder
        }



-- Encoders/Decoders


encode : Session -> String
encode session =
    case session of
        Guest _ ->
            ""

        LoggedIn _ token data ->
            Encode.encode 0 <|
                Encode.object
                    [ ( "accessToken", Encode.string token )
                    , ( "user"
                      , Encode.object
                            [ ( "id", Encode.int data.id )
                            , ( "firstName", Encode.string data.firstName )
                            , ( "lastName", Encode.string data.lastName )
                            , ( "email", Encode.string data.email )
                            , ( "items"
                              , Encode.list
                                    (\item ->
                                        Encode.object
                                            [ ( "id", Encode.int item.id )
                                            , ( "itemId", Encode.string item.itemId )
                                            , ( "accessToken", Encode.string item.accessToken )
                                            ]
                                    )
                                    data.items
                              )
                            ]
                      )
                    ]


userDecoder : Decode.Decoder User
userDecoder =
    Decode.map5 User
        (field "id" int)
        (field "firstName" string)
        (field "lastName" string)
        (field "email" string)
        (field "items" (list itemDecoder))


itemDecoder : Decode.Decoder Item
itemDecoder =
    Decode.map3 Item
        (field "id" int)
        (field "itemId" string)
        (field "accessToken" string)


decoder : Decode.Decoder User
decoder =
    Decode.at [ "user" ] userDecoder
