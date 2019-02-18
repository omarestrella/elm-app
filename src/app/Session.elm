module Session exposing (Item, Session(..), SessionData, User, accessToken, addItemToUser, decoder, encode, getSession, linkItemToUser, navKey, userDecoder)

import Api exposing (Method(..))
import Browser.Navigation as Navigation
import Http
import Json.Decode as Decode exposing (field, int, list, string)
import Json.Decode.Pipeline exposing (hardcoded, optional, required)
import Json.Encode as Encode


type Session
    = Guest Navigation.Key
    | LoggedIn Navigation.Key SessionData


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


type alias Balance =
    { available : Maybe Float
    , current : Float
    , isoCurrencyCode : String
    , limit : Maybe Float
    }


type alias Account =
    { accountId : String
    , balances : Maybe Balance
    , name : String
    , mask : String
    , officialName : String
    , type_ : String
    , subtype : String
    }


type alias SessionData =
    { user : User
    , accessToken : String
    , accounts : List Account
    }



-- Utils


addItemToUser : Session -> Item -> Session
addItemToUser session item =
    case session of
        Guest _ ->
            session

        LoggedIn key sessionData ->
            let
                user =
                    sessionData.user

                updatedUser =
                    { user | items = user.items ++ [ item ] }
            in
            LoggedIn key { sessionData | user = user }


navKey : Session -> Navigation.Key
navKey session =
    case session of
        Guest key ->
            key

        LoggedIn key _ ->
            key


accessToken : Session -> String
accessToken session =
    case session of
        Guest _ ->
            ""

        LoggedIn _ data ->
            data.accessToken


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

        LoggedIn _ data ->
            Encode.encode 0 <|
                Encode.object
                    [ ( "accessToken", Encode.string data.accessToken )
                    , ( "user"
                      , Encode.object
                            [ ( "id", Encode.int data.user.id )
                            , ( "firstName", Encode.string data.user.firstName )
                            , ( "lastName", Encode.string data.user.lastName )
                            , ( "email", Encode.string data.user.email )
                            , ( "items"
                              , Encode.list
                                    (\item ->
                                        Encode.object
                                            [ ( "id", Encode.int item.id )
                                            , ( "itemId", Encode.string item.itemId )
                                            , ( "accessToken", Encode.string item.accessToken )
                                            ]
                                    )
                                    data.user.items
                              )
                            ]
                      )
                    ]


userDecoder : Decode.Decoder User
userDecoder =
    Decode.succeed User
        |> required "id" int
        |> required "firstName" string
        |> required "lastName" string
        |> required "email" string
        |> required "items" (list itemDecoder)


itemDecoder : Decode.Decoder Item
itemDecoder =
    Decode.map3 Item
        (field "id" int)
        (field "itemId" string)
        (field "accessToken" string)


balanceDecoder : Decode.Decoder Balance
balanceDecoder =
    Decode.succeed Balance
        |> optional "available" (Decode.maybe Decode.float) Nothing
        |> required "current" Decode.float
        |> required "iso_currency_code" Decode.string
        |> optional "limit" (Decode.maybe Decode.float) Nothing


accountDecoder : Decode.Decoder Account
accountDecoder =
    Decode.succeed Account
        |> required "account_id" Decode.string
        |> optional "balances" (Decode.maybe balanceDecoder) Nothing
        |> required "name" Decode.string
        |> required "mask" Decode.string
        |> required "official_name" Decode.string
        |> required "type" Decode.string
        |> required "subtype" Decode.string


decoder : Decode.Decoder User
decoder =
    Decode.at [ "user" ] userDecoder
