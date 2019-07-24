port module Session exposing (Account(..), Item, Msg, Session(..), SessionData, User, accessToken, accountTokens, addAccountView, addAccountsToSession, addItemToUser, allAccounts, decoder, encode, getItemAccounts, getSession, linkItemToUser, loadAccounts, loadItems, makeSessionData, navKey, subscriptions, update, userDecoder)

import Api exposing (Method(..))
import Browser.Navigation as Navigation
import Component.Button as Button
import Error exposing (PlaidError, plaidErrorDecoder)
import Html.Styled exposing (Html, div, text)
import Html.Styled.Attributes
import Html.Styled.Events exposing (onClick)
import Http
import Json.Decode as Decode exposing (field, int, list, string)
import Json.Decode.Pipeline exposing (hardcoded, optional, required, requiredAt)
import Json.Encode as Encode
import Link exposing (Link, LinkResponse(..), linkResponseDecoder)
import Url.Builder exposing (Root(..))


type Session
    = Guest Navigation.Key
    | LoggedIn Navigation.Key SessionData


type alias User =
    { id : String
    , firstName : String
    , lastName : String
    , email : String
    , accountTokens : List String
    }


type alias Item =
    { id : String
    , itemId : String
    , accessToken : String
    , publicToken : String
    , accounts : List ItemAccount
    , institutionName : String
    }


type alias Balance =
    { available : Maybe Float
    , current : Float
    , isoCurrencyCode : String
    , limit : Maybe Float
    }


type alias ItemAccount =
    -- An ItemAccount is a short form of the larger account
    -- model
    { id : String
    , accountId : String
    , name : String
    , mask : String
    , itemId : String
    }


type AccountType
    = Brokerage
    | Credit
    | Depository
    | Loan
    | Other
    | UnknownAccountType


type alias AccountDetail =
    { accountId : String
    , balances : Maybe Balance
    , name : String
    , mask : String
    , officialName : Maybe String
    , type_ : AccountType
    , subtype : String
    }


type Account
    = AccountSuccess String (List AccountDetail)
    | AccountError String PlaidError
    | UnknownAccountStatus


type alias SessionData =
    { user : User
    , accessToken : String
    , accounts : List Account
    , items : List Item
    , linkResponse : LinkResponse
    }


type Msg
    = GotAccounts (Result Http.Error (List Account))
    | GotItems (Result Http.Error (List Item))
    | StartLink
    | LinkResponseMsg (Result Decode.Error LinkResponse)
    | HandleItemLink (Result Http.Error Item)


makeSessionData : String -> User -> SessionData
makeSessionData token user =
    { accessToken = token
    , user = user
    , accounts = []
    , items = []
    , linkResponse = NoLink
    }


update : Msg -> Session -> ( Session, Cmd Msg )
update msg session =
    case msg of
        GotAccounts response ->
            case response of
                Ok accounts ->
                    ( addAccountsToSession session accounts, Cmd.none )

                Err err ->
                    ( session, Cmd.none )

        GotItems response ->
            case response of
                Ok items ->
                    ( addItemsToSession session items, Cmd.none )

                Err _ ->
                    ( session, Cmd.none )

        StartLink ->
            ( session, plaidLink "StartLink" )

        LinkResponseMsg response ->
            let
                token =
                    accessToken session
            in
            case response of
                Ok linkData ->
                    case linkData of
                        LinkSuccess link ->
                            ( session
                            , linkItemToUser HandleItemLink token link
                            )

                        LinkError errors ->
                            -- MARK: graceful error handling
                            ( session, Cmd.none )

                        NoLink ->
                            ( session, Cmd.none )

                Err _ ->
                    -- MARK: graceful error handling
                    ( session, Cmd.none )

        HandleItemLink response ->
            case response of
                Ok item ->
                    ( addItemToUser session item
                    , Cmd.none
                    )

                Err _ ->
                    ( session, Cmd.none )



-- Utils


accountTypeStringToValue : String -> AccountType
accountTypeStringToValue str =
    case str of
        "brokerage" ->
            Brokerage

        "credit" ->
            Credit

        "depository" ->
            Depository

        "loan" ->
            Loan

        "other" ->
            Other

        _ ->
            UnknownAccountType


addItemToUser : Session -> Item -> Session
addItemToUser session item =
    case session of
        Guest _ ->
            session

        LoggedIn key sessionData ->
            LoggedIn key { sessionData | items = item :: sessionData.items }


addItemsToSession : Session -> List Item -> Session
addItemsToSession session items =
    case session of
        Guest _ ->
            session

        LoggedIn key data ->
            LoggedIn key { data | items = data.items ++ items }


addAccountsToSession : Session -> List Account -> Session
addAccountsToSession session accounts =
    case session of
        Guest _ ->
            session

        LoggedIn key sessionData ->
            LoggedIn key { sessionData | accounts = sessionData.accounts ++ accounts }


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


accountTokens : Session -> List String
accountTokens session =
    case session of
        Guest _ ->
            []

        LoggedIn _ data ->
            data.user.accountTokens


allAccounts : Session -> List Account
allAccounts session =
    case session of
        Guest _ ->
            []

        LoggedIn _ data ->
            data.accounts


allItems : Session -> List Item
allItems session =
    case session of
        Guest _ ->
            []

        LoggedIn _ data ->
            data.items


itemForAccessToken : Session -> String -> Maybe Item
itemForAccessToken session token =
    let
        items =
            allItems session
    in
    List.filter (\item -> item.accessToken == token) items
        |> List.head


accountForAccessToken : Session -> String -> Maybe Account
accountForAccessToken session token =
    let
        accounts =
            allAccounts session
    in
    Nothing


itemAccountsForAccessToken : Session -> String -> List ItemAccount
itemAccountsForAccessToken session token =
    Maybe.withDefault []
        (itemForAccessToken session token
            |> Maybe.andThen (\item -> Just item.accounts)
        )



-- Account Helpers


getItemAccounts : Session -> List ItemAccount
getItemAccounts session =
    let
        accessTokens =
            accountTokens session
    in
    List.concatMap (\token -> itemAccountsForAccessToken session token)
        accessTokens



-- API Requests


getSession : (Result Http.Error User -> msg) -> String -> Cmd msg
getSession msg token =
    Api.request
        { url = "/auth/session"
        , accessToken = Just token
        , method = POST
        , body = Nothing
        , handler = Http.expectJson msg decoder
        }


linkItemToUser : (Result Http.Error Item -> msg) -> String -> Link -> Cmd msg
linkItemToUser msg token link =
    let
        accountEncoder =
            \account ->
                Encode.object
                    [ ( "id", Encode.string account.id )
                    , ( "name", Encode.string account.name )
                    , ( "mask", Encode.string account.mask )
                    ]

        body =
            Encode.object
                [ ( "publicToken", Encode.string link.publicToken )
                , ( "institutionId", Encode.string link.institution.institutionId )
                , ( "accounts", Encode.list accountEncoder link.accounts )
                ]
    in
    Api.request
        { url = "/items"
        , accessToken = Just token
        , method = POST
        , body = Just (Http.jsonBody body)
        , handler = Http.expectJson msg itemDecoder
        }


loadAccounts : String -> List String -> Cmd Msg
loadAccounts token accessTokens =
    let
        url =
            Api.accessTokensUrl accessTokens "accounts"
    in
    Api.request
        { url = url
        , accessToken = Just token
        , method = GET
        , body = Nothing
        , handler = Http.expectJson GotAccounts (Decode.list accountDecoder)
        }


loadItems : String -> Cmd Msg
loadItems token =
    Api.request
        { url = "/items"
        , accessToken = Just token
        , method = GET
        , body = Nothing
        , handler = Http.expectJson GotItems (Decode.list itemDecoder)
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
                            [ ( "id", Encode.string data.user.id )
                            , ( "firstName", Encode.string data.user.firstName )
                            , ( "lastName", Encode.string data.user.lastName )
                            , ( "email", Encode.string data.user.email )
                            , ( "items"
                              , Encode.list
                                    (\item ->
                                        Encode.object
                                            [ ( "id", Encode.string item.id )
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
    Decode.succeed User
        |> required "id" string
        |> required "firstName" string
        |> required "lastName" string
        |> required "email" string
        |> required "tokens" (list (Decode.field "accessToken" string))


itemDecoder : Decode.Decoder Item
itemDecoder =
    Decode.succeed Item
        |> required "id" string
        |> required "itemId" string
        |> required "accessToken" string
        |> required "publicToken" string
        |> required "accounts" (list itemAccountDecoder)
        |> required "institutionName" string


itemAccountDecoder : Decode.Decoder ItemAccount
itemAccountDecoder =
    Decode.succeed ItemAccount
        |> required "id" string
        |> required "accountId" string
        |> required "name" string
        |> required "mask" string
        |> required "itemId" string


balanceDecoder : Decode.Decoder Balance
balanceDecoder =
    Decode.succeed Balance
        |> optional "available" (Decode.maybe Decode.float) Nothing
        |> required "current" Decode.float
        |> required "iso_currency_code" Decode.string
        |> optional "limit" (Decode.maybe Decode.float) Nothing


accountDecoder : Decode.Decoder Account
accountDecoder =
    Decode.oneOf
        [ accountErrorDecoder
        , Decode.field "token" Decode.string
            |> Decode.andThen
                (\token ->
                    Decode.field "accounts" (Decode.list accountDetailDecoder)
                        |> Decode.map (AccountSuccess token)
                )
        , Decode.succeed UnknownAccountStatus
        ]


accountErrorDecoder : Decode.Decoder Account
accountErrorDecoder =
    Decode.field "token" Decode.string
        |> Decode.andThen
            (\token ->
                plaidErrorDecoder |> Decode.map (AccountError token)
            )


accountDetailDecoder : Decode.Decoder AccountDetail
accountDetailDecoder =
    Decode.field "type" Decode.string
        |> Decode.andThen
            (\type_ ->
                Decode.succeed AccountDetail
                    |> required "account_id" Decode.string
                    |> optional "balances" (Decode.maybe balanceDecoder) Nothing
                    |> required "name" Decode.string
                    |> required "mask" Decode.string
                    |> optional "official_name" (Decode.maybe Decode.string) Nothing
                    |> hardcoded (accountTypeStringToValue type_)
                    |> required "subtype" Decode.string
            )


decoder : Decode.Decoder User
decoder =
    Decode.at [ "user" ] userDecoder



-- Views


addAccountView : Html Msg
addAccountView =
    div []
        [ Button.primary [ onClick StartLink ] [ text "Add account" ] ]



-- Subscriptions


port linkResponse : (Decode.Value -> msg) -> Sub msg


port plaidLink : String -> Cmd msg


subscriptions : Sub Msg
subscriptions =
    linkResponse
        (\value ->
            LinkResponseMsg (Decode.decodeValue linkResponseDecoder value)
        )
